# Macaulean

## How to install Macaulay2

### macOS
```
brew install Macaulay2/tap/M2
```

### Ubuntu
```
sudo add-apt-repository ppa:macaulay2/macaulay2
sudo apt install macaulay2
```

### Other systems
See the [wiki](https://github.com/Macaulay2/M2/wiki).

## Links

* [LeanM2](https://github.com/riyazahuja/lean-m2) (+ [fork](https://github.com/mattrobball/leanm2_fork))
* [Lean-Oscar](https://github.com/todbeibrot/Lean-Oscar)
* [mrdi file format](https://arxiv.org/abs/2309.00465)

## Loading the JSONRPC/MRDI packages

Suppose you have cloned the Macaulean repository to `/path/to/Macaulean`.
Then run:

```m2
path = append(path, "/path/to/Macaulean/m2")
needsPackage "JSONRPC"
needsPackage "MRDI"
```

