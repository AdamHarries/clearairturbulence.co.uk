---
title: Last (git) modification time in Hakyll
tags: haskell, hakyll, git
date: 2018-09-03
abstract: Hakyll gives you lots of lovely defaults, but some things are harder to come by. This post provides a brief description of how to add "last modification time" metadata to each post.
---

While working on this site, I wanted to list metadata for each page describing both the time the page was originally written, and the date on which it was last modified. The first is fairly easy, and is included in Hakyll by default: Given a page at a path such as: `writing/2018-08-03-last-git-modification-time-in-haskyll.markdown`, the Hakyll compiler will extract the `2018-08-03` portion of the filepath, and fill in the `$date$` portion of the [template](https://github.com/AdamHarries/clearairturbulence.co.uk/blob/master/templates/post.html) with the formatted date. 

The second, listing the modification time, was somewhat more difficult, and took a bit of hacking to get working nicely. To start with, we need some way to *extract* the modification times. This is performed by the following method: 

```haskell
buildModifications ::  Pattern -> Rules [(Identifier, String)]
buildModifications pattern = do 
    ids <- getMatches pattern
    pairs <- preprocess $ foldM getLastModified [] ids
    return pairs
    where 
        getLastModified l id' = do
            t <- readProcess "git" 
                ["log", "-1", "--format=%ad", "--date=format:%b %d, %Y", (toFilePath id')] ""
            return $ (id', t) : l
```

Essentially, this function takes a [pattern](https://jaspervdj.be/hakyll/reference/Hakyll-Core-Identifier-Pattern.html) (describing a list of files) and, for each of them, runs the `git` executable to find when the file was last modified *according to git*. We don't really care about the *actual* modification time, but when we officially "committed" the changes to the site. Finally, we wrap the result in a `Rules` monad, which implements the `MonadMetadata` typeclass, smoothly integrating pattern matching and IO (through judicious use of `preprocess`).

Once we have the modification times, we need to use a [context](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html) to describe how, and when we should substitute in the times: 

```haskell
modificationCtx :: [(Identifier, String)] -> Context String 
modificationCtx modificationTimes = field "lastModified" $ \item -> do
    let time = find (\x -> (fst x) == (itemIdentifier item)) modificationTimes >>= return . snd 
    return $ fromMaybe "no recent modifications" $ time
```

This method is fairly straightforward: it takes a given item, looks up the `lastModified` field in the body, looks up the time (if there is one) of the identifier of the item, and returns a context replacing `lastModified` with the lookup operation on the item. 

For fun, here's a simple context where we can simply insert the path of the item's identifier where requested: 

```haskell
githubCtx :: Context String 
githubCtx = field "filepath" $ \item -> do 
    let path = (toFilePath . itemIdentifier) item
    return $ path
```

To see them both in action, check out the [site source](https://github.com/AdamHarries/clearairturbulence.co.uk/blob/master/site.hs).
