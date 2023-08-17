# VirtualECU

This project implements an ECU simulator completely in software. The simulated communication is initially limited to standardized OBD-II requests, but can easily be extended to support additional standards or even non-standard protocols.

Its CAN and ISOTP layers are powered by the [JavaCAN](https://github.com/pschichtel/JavaCAN) project using the Linux kernel's SocketCAN API. The OBD communication protocol implementation is provided by [obd4s](https://github.com/pschichtel/obd4s), a Scala library built on top of JavaCAN to provide proper standards compliant OBD-II communication.

## What works so far

* Simulating multiple ECUs
* Functional and physical addressing, for SFF, EFF and mixed frames
* Configuring time series functions per service and subfunction either by providing a compiled Java class or by using Scala expressions directly in the configuration
* Invalid requests will result in appropriate error responses
* Can be used with real CAN devices or a virtual device

## How to Run

The project takes 2 required parameters:

1. the CAN device name
2. the configuration file (see config.yaml in this repository for an example)

Executing the program will also print the usage.

## The Configuration

A simple example would look like this:

```yaml
controllers:
  "7E0":
    name: Engine
    services:
      "01":
        name: Live Data
        action:
          generator: "constant(1.0) andThen unsignedInteger(1)"
        parameters:
          "05":
            name: Coolant Temperature
            action:
              generator: "linear(1, 8.minutes)"
```

The `controllers` field is a map from functional ECU CAN address (in hex) to ECU objects.

Each ECU consists of a `name` and a list of services. The `services` field is a map from service ID byte (in hex) to service objects.

Each service consists of a `name`, an optional action and parameters. The `parameters` field is a map from parameter id byte (in hex) to parameters.

Each parameter consists of a `name` and an action.

Actions have either a `generator` or an `explicit`. `generator` is a scala expression of type `Double => Array[Byte]`, so a function that takes a doubel and produces a byte array. All functions defined in `Functions.scala` are implicitly available to these expressions. `explicit` is a fully qualified class name of a class that implements the `Action` interface.
