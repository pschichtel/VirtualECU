# VirtualECU

This project implements a ECU simulator completely in software. The simulated communication is initially limited to standardized OBD-II requests, but can easily extended to support additional standards or even non-standard protocols.

Its CAN and ISOTP layers are powered by the [JavaCAN](https://github.com/pschichtel/JavaCAN) project using the Linux kernel's SocketCAN API. The OBD communication protocol implementation is provided by [obd4s](https://github.com/pschichtel/obd4s), a Scala library built on top of JavaCAN to provide proper standards compliant OBD-II communication.

## What works so far

* Simulating multiple ECUs
* Functional and physical addressing, for SFF, EFF and mixed frames
* Configuring time series functions per service and subfunction either bei providing a compiled Java class or by using Scala expressions directly in the configuration
* Invalid requests will result in appropriate error responses
* Can be used with real CAN devices or a virtual device
