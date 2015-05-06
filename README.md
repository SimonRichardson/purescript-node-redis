# purescript-node-redis

An library taking advantage of purescript-aff to enable pain-free asynchronous 
Redis commands.

## Getting Started

### Installation

```bash
bower install purescript-node-redis
```

### Introduction

You can construct commands with the `redis` functions:

```purescript
main = launchAff $ do
  liftEff $ unit
```

### Module documentation

Coming soon.

### General

Note: This is a wrapper around the _node_ library and as such is limited to that
very fact. It's possible to write a better interface (with more time) that 
removes the node library and talks directly to the database.