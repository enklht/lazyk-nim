# lazyk-nim

> **Note:** This is a partial implementation and only supports SKI combinators.

A Lazy K interpreter implementation in Nim. [Lazy K](https://esolangs.org/wiki/Lazy_K) is an esoteric programming language based on combinatory logic.

## Usage

To run a Lazy K program:

```bash
nimble run -d:release -- path/to/your/program.lazy
```

For example, to run the prime numbers generator:

```bash
nimble run -d:release -- example/primes.lazy
```
