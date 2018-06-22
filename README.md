# LambdaCube Font Engine

This is a work-in-progress project to provide font rendering capabilities for [LambdaCube 3D](https://github.com/lambdacube3d/lambdacube-edsl).

The basic idea behind the composite distance field approximation method is described in a [blog post](http://lambdacube3d.wordpress.com/2014/11/12/playing-around-with-font-rendering/).

## Compile & Run:

To compile you will need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack build
stack exec -- lafonten-test Ubuntu-R.ttf
```

![Lafonten Demo](lafonten-demo.png)
