use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Wake};
use crossbeam::channel::{unbounded, Receiver, Sender};
use futures::task::{ArcWake, waker_ref};
use parking_lot::Mutex;
use smallvec::SmallVec;

type BoxFuture = Pin<Box<dyn Future<Output = ()> + Send>>;

struct Task {
    future: Mutex<BoxFuture>,
    sender: Sender<Arc<Task>>,
    stats: Arc<TaskStats>,
}

struct TaskStats {
    polls: AtomicUsize,
    wake_count: AtomicUsize,
    last_poll: AtomicU64,
    creation_time: u64,
}

impl ArcWake for Task {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        arc_self.stats.wake_count.fetch_add(1, Ordering::Relaxed);
        arc_self.sender.send(arc_self.clone()).unwrap();
    }
}

pub struct Runtime {
    sender: Sender<Arc<Task>>,
    receiver: Receiver<Arc<Task>>,
    thread_pool: ThreadPool,
    task_stats: Arc<RuntimeStats>,
}

impl Runtime {
    pub fn new(threads: usize) -> Self {
        let (sender, receiver) = unbounded();
        let thread_pool = ThreadPool::new(threads);
        
        Self {
            sender,
            receiver,
            thread_pool,
            task_stats: Arc::new(RuntimeStats::new()),
        }
    }

    pub fn spawn<F>(&self, future: F) 
    where
        F: Future<Output = ()> + Send + 'static,
    {
        let stats = Arc::new(TaskStats {
            polls: AtomicUsize::new(0),
            wake_count: AtomicUsize::new(0),
            last_poll: AtomicU64::new(0),
            creation_time: timestamp(),
        });

        let task = Arc::new(Task {
            future: Mutex::new(Box::pin(future)),
            sender: self.sender.clone(),
            stats,
        });

        self.sender.send(task).unwrap();
    }

    pub fn run(&self) {
        while let Ok(task) = self.receiver.recv() {
            let waker = waker_ref(&task);
            let mut context = Context::from_waker(&waker);
            
            let mut future = task.future.lock();
            let poll_start = timestamp();
            
            match future.as_mut().poll(&mut context) {
                Poll::Ready(()) => {
                    self.task_stats.completed_tasks.fetch_add(1, Ordering::Relaxed);
                },
                Poll::Pending => {
                    task.stats.polls.fetch_add(1, Ordering::Relaxed);
                    task.stats.last_poll.store(poll_start, Ordering::Relaxed);
                }
            }

            let poll_duration = timestamp() - poll_start;
            self.task_stats.update_poll_stats(poll_duration);
        }
    }

    pub fn metrics(&self) -> RuntimeMetrics {
        RuntimeMetrics {
            active_tasks: self.task_stats.active_tasks.load(Ordering::Relaxed),
            completed_tasks: self.task_stats.completed_tasks.load(Ordering::Relaxed),
            avg_poll_duration: self.task_stats.avg_poll_duration(),
            max_poll_duration: self.task_stats.max_poll_duration.load(Ordering::Relaxed),
            total_polls: self.task_stats.total_polls.load(Ordering::Relaxed),
        }
    }
}

struct RuntimeStats {
    active_tasks: AtomicUsize,
    completed_tasks: AtomicUsize,
    total_polls: AtomicUsize,
    total_poll_duration: AtomicU64,
    max_poll_duration: AtomicU64,
}

impl RuntimeStats {
    fn new() -> Self {
        Self {
            active_tasks: AtomicUsize::new(0),
            completed_tasks: AtomicUsize::new(0),
            total_polls: AtomicUsize::new(0),
            total_poll_duration: AtomicU64::new(0),
            max_poll_duration: AtomicU64::new(0),
        }
    }

    fn update_poll_stats(&self, duration: u64) {
        self.total_polls.fetch_add(1, Ordering::Relaxed);
        self.total_poll_duration.fetch_add(duration, Ordering::Relaxed);
        
        let mut max = self.max_poll_duration.load(Ordering::Relaxed);
        while duration > max {
            match self.max_poll_duration.compare_exchange_weak(
                max, duration, Ordering::Relaxed, Ordering::Relaxed
            ) {
                Ok(_) => break,
                Err(current) => max = current,
            }
        }
    }

    fn avg_poll_duration(&self) -> u64 {
        let total = self.total_poll_duration.load(Ordering::Relaxed);
        let count = self.total_polls.load(Ordering::Relaxed);
        if count > 0 {
            total / count as u64
        } else {
            0
        }
    }
}

fn timestamp() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_micros() as u64
} 