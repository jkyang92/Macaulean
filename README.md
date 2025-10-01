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

## Loading the JSONRPC/MRDI packages

Suppose you have cloned the Macaulean repository to `/path/to/Macaulean`.
Then run:

```m2
path = append(path, "/path/to/Macaulean/m2")
needsPackage "JSONRPC"
needsPackage "MRDI"
```

