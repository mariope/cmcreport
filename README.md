# cmcparse v0.1.0.9000

Parse and analyze a exported operations report from CMC Markets CfD broker

## Getting Started

First of all. This package has not any utility for you unless you have a real or 
demo account with the broker CMC Markets.

The feed for cmcparse package is a exported report from CMC, this report can be
any range of dates. CMC writes one row for each state of operation, 
having at least two rows by operation.

**cmcparse** read a csv file exported from CMC and it generates a data.frame
with one row for each operation adding useful information for analysis 
operations performance.

- Parse a exported report from CMC Markets
- Generate a data.frame with the operations one by one
- Calculate the performance of operations
- Show graphics performance

### Prerequisities

If you install from devtools you do not need to do anything else.

```
> devtools::install_github("mariope/cmcparse")
```

## Running cmcparse

The function available are:

### readCMCreport("../cmcreports/History20160930.csv")

Read a CMC report and generate a data.frame by operations.

```
> operations <- readCMCreport("../cmcreports/History20160930.csv")
```

### performanceCMCreport(operations)

Make operations performance analysis

```
> performanceCMCreport(operations)
```

### displayCMCperformance(operations) -TODO

Display operations performance analysis

```
> displayCMCperformance(operations)
```

## Built With

* R v3.3.0
* R Studio - version 0.99.903

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/mariope/cmcparse/tags). 

## Authors

* **Mario Pisa** - *Initial work* - [cmcparse](https://github.com/mariope/cmcparse)

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

* To Open Source community.
