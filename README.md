# LambdaCube Font Engine

This is a work-in-progress project to provide font rendering capabilities for [LambdaCube 3D](https://github.com/csabahruska/lc-dsl). Quick instructions:

1. `cabal install` this library.
2. `cabal install GLFW-b`
3. `ghci src/test/HelloWorld.hs`
4. `:main <path-to-ttf-file> <pixels-per-em>` (the second parameter is optional, defaults to 72)

The basic idea behind the composite distance field approximation method is described in a [blog post](http://lambdacube3d.wordpress.com/2014/11/12/playing-around-with-font-rendering/).
