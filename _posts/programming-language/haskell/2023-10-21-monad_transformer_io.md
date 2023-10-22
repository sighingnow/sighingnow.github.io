---
title: I/O Error Handling Gracefully with Monad Transformer 
author: Tao He
date: 2023-10-21
tag: [Haskell]
category: Programming Languages
layout: post
---

The monad transformers is design to stacking monad to represent complex effects
in Haskell. The [transformer][1] and [mtl][2] package provides a set of monad
classes to help combining (stacking) multiple monads together. In real world
applications, I/O is unavoidable, and its tricky to execute stacked monad in
the IO monad.

<!--more-->

Why bother?
-----------

Imaging we have a database query function which is actually a foreign function:

```haskell
type Result = Int

type DBError = Int

foreign import ccall unsafe "query"
    query :: Connection -> CString -> Ptr Result -> IO DBError
```

It results a status code where 0 means success, and non-zero means failure. Note
that this C function use the argument `Ptr Result` to return the query result,
where the pointer can be allocated by functions like `alloca` in Haskell.
We would like to capture the error and propogate it gracefully. We define a monad
to represent the effect:

```haskell
newtype DBMonad a = DBMonad {runDBMonad :: ExceptT DBError IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError DBError)
```

Then we could use the monad to wrap the foreign function:

```haskell
queryDB :: Connection -> String -> Ptr Result -> DBMonad ()
queryDB conn sql r = do
    code <- liftIO $ withCString sql $ \sql' -> query conn sql' r
    when (code /= 0) $ throwError code
```

and use it like

```haskell
work :: Connection -> DBMonad Result
work conn = do
    ptr <- liftIO $ mallocBytes (sizeOf (undefined :: Result))
    queryDB conn "SELECT * FROM users" ptr
    liftIO $ peek ptr

entry :: Connection -> IO (Either DBError Result)
entry conn = runExceptT $ runDBMonad $ work conn
```

where `runExceptT` returns a `IO (Either DBError ())` value. Looks good, but want
if we want to use `alloca` to allocate the pointer and ensure it been deallocated
when query finished? We would take a try:

```haskell
work :: Connection -> DBMonad Result
work conn = do
    liftIO $ alloca $ \ptr ->
        queryDB conn "SELECT * FROM users" ptr
```

With no surprise, it won't compile.

```
    • Couldn't match expected type: IO Result
                  with actual type: DBMonad ()
    • In the expression: queryDB conn "SELECT * FROM users" ptr
      In the second argument of ‘($)’, namely
        ‘\ ptr -> queryDB conn "SELECT * FROM users" ptr’
      In the second argument of ‘($)’, namely
        ‘alloca $ \ ptr -> queryDB conn "SELECT * FROM users" ptr’
```

Evaluate the monad in I/O
-------------------------

The reason is that `alloca` is a function that expectes an IO action as its
argument, but `queryDB` is a monadic action in `DBMonad`. We need to lift
the monadic action to IO action, **i.e., the reverse of `liftIO`**. Not hard
to implement:

```haskell
runDB :: DBMonad a -> IO (Either DBError a)
runDB = runExceptT . runDBMonad

work :: Connection -> DBMonad Result
work conn = do
    liftIO $ alloca $ \ptr ->
        runDB $ do
            queryDB conn "SELECT * FROM users" ptr
            liftIO $ peek ptr
```

Now it continues complaining:

```
    • Couldn't match type ‘Either DBError Result’ with ‘Int’
      Expected: IO Result
        Actual: IO (Either DBError Result)
    • In the expression:
        runDB
          $ do queryDB conn "SELECT * FROM users" ptr
               liftIO $ peek ptr
      In the second argument of ‘($)’, namely
        ‘\ ptr
           -> runDB
                $ do queryDB conn "SELECT * FROM users" ptr
                     liftIO $ peek ptr’
      In the second argument of ‘($)’, namely
        ‘alloca
           $ \ ptr
               -> runDB
                    $ do queryDB conn "SELECT * FROM users" ptr
                         liftIO $ peek ptr’
```

The `alloca ...` blocks returns an `Either DBError Result` value, while the
outer `liftIO` and `work` expects a value of type `Result`. Fortunately, we
have `liftEither` defined in the [`Control.Monad.Except`][3] module:

```haskell
liftEither :: MonadError e m => Either e a -> m a
```

Combining it with `liftIO`, we get what we want:

```haskell
liftIOEither :: IO (Either DBError a) -> DBMonad a
liftIOEither = liftEither <=< liftIO
```

Using it in `work`:

```diff
 work :: Connection -> DBMonad Result
 work conn = do
-    liftIO $ alloca $ \ptr ->
+    liftIOEither $ alloca $ \ptr ->
         runDB $ do
             queryDB conn "SELECT * FROM users" ptr
             liftIO $ peek ptr
```

Everything works as expected.


[1]: https://hackage.haskell.org/package/transformers
[2]: https://hackage.haskell.org/package/mtl
[3]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Except.html
