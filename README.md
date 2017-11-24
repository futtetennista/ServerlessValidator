# Serverless Validator

Validates [Serverless](https://serverless.com/) YML files.

## Usage

If haskell, stack are installed on you system simply clone this repo and type: `./src/Serverlessvalidator.hs path/to/serverless.yml`. The script leverages stack's [scripting](https://haskell-lang.org/tutorial/stack-script) capabilities that is Stack will take care of downloading all the necessary dependencies and run the validator, just sit back and wait for it to be finished (it might take a bit the first time). For example, if the `runtime` string in `serverless.yml` is not valid, the ouput will be something like:

```
$ ./src/Serverlessvalidator.hs path/to/serverless-wrong-runtime.yml
'serverless-wrong-runtime.yml' is not valid
AesonException "Error in $.provider.runtime: failed to parse field provider: failed to parse field runtime: Unsupported runtime 'nodejs'. Choose one among: 'nodejs4.3', 'java8', 'python2.8'"

```

## Usage with Docker

A Docker executable image is available on Docker Hub, just download it and run it against you `serverless.yml` file: 

``` sh 
docker pull futtetennista/serverless-validator
docker run --rm -v=$(pwd):/tmp -w=/tmp serveless-validator path/to/serverless.yml
```

## Licence

```
MIT License

Copyright (c) 2017 Futtetennista

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

```
