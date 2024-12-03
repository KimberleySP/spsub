package ratelimiter

import (
	"sync"
	"time"
)

type TokenBucket struct {
	rate     float64
	capacity float64
	tokens   float64
	lastTime time.Time
	mu       sync.Mutex
}

func NewTokenBucket(rate, capacity float64) *TokenBucket {
	return &TokenBucket{
		rate:     rate,
		capacity: capacity,
		tokens:   capacity,
		lastTime: time.Now(),
	}
}
