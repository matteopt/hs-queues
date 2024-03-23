# hs-queues

Simple memory queues over TCP for messaging.

```haskell
main :: IO ()
main = do
    forkIO $ runServer
    c <- acquire "localhost"
    push c "my_queue" "hello"
    (Just x) <- pop c "my_queue"
```
