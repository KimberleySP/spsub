package cache.lru

class LRUCache<K, V>(private val capacity: Int) {
    private val cache = LinkedHashMap<K, V>(capacity, 0.75f, true)
    
    @Synchronized
    fun get(key: K): V? = cache[key]
    
    @Synchronized
    fun put(key: K, value: V) {
        cache[key] = value
        if (cache.size > capacity) {
            cache.remove(cache.keys.first())
        }
    }
} 