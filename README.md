# DecentMon: an OCaml Benchmark for Decentralised Monitoring of LTL Formulae

Current Version: 3.1.

---

## 1 Description

### Overview

DecenMon is an OCaml Benchmark for Decentralized Monitoring of LTL formulae.
DcentMont is written in the functional programming language OCaml.

For a full description of the underlying algorithms and principles, we refer the reader to the following journal publications:
> * Andreas Bauer, Yliès Falcone:
Decentralised LTL monitoring. Formal Methods in System Design 48(1-2): 46-93 (2016)
> * Christian Colombo, Yliès Falcone:
Organising LTL monitors over distributed systems with a global clock. Formal Methods in System Design 49(1-2): 109-158 (2016)

Earlier versions of the benchmark were released with the following conference publications:
> * Andreas Klaus Bauer, Yliès Falcone:
Decentralised LTL Monitoring. FM 2012: 85-100
> * Christian Colombo, Yliès Falcone:
Organising LTL Monitors over Distributed Systems with a Global Clock. RV 2014: 140-155

DecentMon takes as input:
- some LTL formulae to be monitored or some LTL Specification patterns (see the Specification Patterns Website);
- some traces against the formulae are monitored; an architecture given by a distributed alphabet indicating how components are organised and distributed in the system.

Note that the two inputs are not mandatory. One can simply indicate a size for a formula that will be automatically generated. Similarly, a size can be indicated for a trace to be automatically generated.

DecentMon then outputs:
- verdicts for the monitored formulae against the input traces;
- some monitoring statistics such as the number of messages exchanged by the local monitors and the length of the trace needed to reach a verdict.

LTL formulae are analysed in two different modes:
1. by using the decentralised approach described above (i.e., each trace is read by a separate monitor), and
2. by merging the traces to a single, global trace and then using a “central monitor” for the formula (i.e., all local monitors send their respective events to the central monitor who makes the decisions regarding the trace).

## 2 Requirements

DecentMon requires GNU Make, [OCaml](https://ocaml.org) and a few OCaml extensions which can be easily installed with [opam](https://opam.ocaml.org).
Additional required packages are:
  - oasis
  - campl4
  - batteries
  - ocamlbuild
  - ocamlfind

DecentMon has been built as end of 2021 with OCaml 4.02. Later versions of OCaml seem not compatible with camlp4.

## 3. Installation

If you wan to try DecentMon without installing it, you can:
- run it within a provided virtual machine where DecentMon is preinstalled;
- a Docker file allowing to build a Docker image and run DecentMon within a container.

Otherwise, in Section 3.3, you can find instructions to install DecentMon on your machine.

### 3.1 Pre-installed version from the provided virtual machine

There is a Ubuntu [Virtual Box](https://www.virtualbox.org) virtual machine within the `decentmon_vm` directory of the repository.
As the image is going to be outdated, make sure to update it with latest security patches.

### 3.2 Installation with Docker

In directory `docker`, there is a Dockerfile to build a docker image.
Once you have Docker and GNU make installed on your machine, simply run:
```
make docker
```
This may take a few minutes (depending on the images that will be downloaded).
Then to run the container, and compile DecentMon, run:
```
make run
```
Within the container, if you modify the source files (in src/), and need to recompile DecentMon, run:
```
ocaml setup.ml -build
```

### 3.3 Installation on your machine


Otherwise, to install DecenMon, please follow the below steps.

1. [Install OPAM](https://opam.ocaml.org/doc/Install.html).
2. Install a C compiler (to install OCaml).
3. [Install OCaml](https://ocaml.org/docs/install.html).
4. [Configure oasis](https://ocaml.org/learn/tutorials/setting_up_with_oasis.html).
For this, the current oasis configutation file is used:
```
OASISFormat: 0.4
Name:        DecentMon
Version:     3.1
Synopsis:    Decentralised Monitoring of LTL formulae
Authors:     Ylies Falcone, Univ. Grenoble Alpes, Inria
License:     GPL-3

Executable decentmon
  Path:       src
  BuildTools: ocamlbuild
  MainIs:     decentmon.ml
  CompiledObject: best
  BuildDepends: batteries, camlp4
```
- `OASISFormat` should be left as is;
- `Name`, `Version`, `Synopsis`, `Authors` and `Licence` are for information purposes
- `Path`indicates the relative path to the directory containing the source files
- `BuildTools` should be left as is
- `MainIs` indicates the main file (entry point of the compilation process)
- `CompileObject` indicates whether to compile in bytecode or native format. It can take the values `bytecode` (compile in bytecode), `native` (compile in native) or `best` (compile in native if possible otherwise in bytecode).

The provided configutation can left as is or you can amend the file as per your needs.

5. Run the Make install target: 
```
make install
```
6. If you modify the source files (in src/), you can recompile decentmon by running the Make decentmon target: 
```
make decentmon
```
## 4 Running DecentMon

To run the benchmark, it is simply needed to run the executable file "decentmon" (either native or bytecode) with appropriate options.
Two kinds of benchmarks are implemented for now. Options required for both kinds are described below.

### Benchmarking randomly generated LTL formulae

In this form of benchmark, one indicates either a specific size for the formulae or a maximum size for the formulae to be tested. Those cases correspond to options:
- `-sf [integer>0]` to indicate a specific size,
- `-msf [integer>0]` to indicate a maximum size.

In the later case, DecentMon will run benchmarks for each formula size from 1 to the desired size. Note that the size of formulae is measured in DecentMon in terms of operator entailment inside the formula. This choice, made after some experiments, reflects the intuitive idea that decentralized monitoring becomes heavier when operator entailment grows (instead of the number of symbols, for example).

Then several other options are mandatory:
- `-n [integer>0]` to indicate the number of formulae to be monitored for each formula size,
- `-st [integer>0]` to indicate the size of the trace against which formulae will be monitored,
- `-dalpha [string_representation_of_a_distributed_alphabet]` to indicate the distributed alphabet to consider during the benchmark (See Section input formats).

### Benchmark for LTL specification patterns

In this benchmark, one indicates one or several LTL specification patterns to be tested (see the [Specification pattern Website](http://patterns.projects.cis.ksu.edu/)). For convenience, the various kinds of formulae for each LTL specification patterns are recalled in Appendix B. The options used to select patterns are:

- `-abs [boolean]`: to select the *Absence* specification pattern for testing when the boolean is true;
- `-exis [boolean]`: to select the *Existence* specification pattern for testing when the boolean is true;
- `-univ [boolean]`: to select the *Universality* specification pattern for testing when the boolean is true;
- `-prec [boolean]`: to select the *Precedence* specification pattern for testing when the boolean is true;
- `-resp [boolean]`: to select the *Response* specification pattern for testing when the boolean is true;
- `-prec [boolean]`: to select the *Precedence* specification pattern for testing when the boolean is true;
- `-precc [boolean]`: to select the *Precedence Chain* specification pattern for testing when the boolean is true;
- `-respc [boolean]`: to select the *Response Chain* specification pattern for testing when the boolean is true;
- `-consc [boolean]`: to select the *Contrained Chain* specification pattern for testing when the boolean is true.

Not mentioning a specification patterns amounts to setting its Boolean flag to false.

Similarly, several other options are mandatory:
- `	-st [integer>0]` to indicate the size of the trace against which formulae will be monitored,
- `	-dalpha [string_representation_of_a_distributed_alphabet]` to indicate the distributed alphabet to consider during the benchmark (See Section input formats).

Then, one can choose the metrics and statistics that shall be displayed by using one of the following options:
- `-prt_full [bool]` to indicate that all statistics (number of messages, trace lengths and their ratio, but also the maximal and average delay) shall be displayed,
- `-prt_trace_mess [bool]` to indicate that trace and number of messages statistics") shall be displayed,
- `-prt_delay [bool]` to indicate that delay statistics shall be displayed.

## Appendix A: Input formats

We describe below some input formats that shall be used when submitting inputs for benchmark to DecentMon.

### Distributed alphabet

They are represented by a quoted string. Distributed alphabets are delimited by curly braces. Inside a distributed alphabet the symbol `|` (resp. `,`) is used to indicate component separation (reps. separate symbols inside a component).
For instance `{a1,a2|b1,b2|c1,c2}` denotes a 3-component architecture. On component 1 (reps. 2,3), the local alphabet contains symbols a1 and a2 (resp. b1 and b2, c1 and c2).

### Appendix B: LTL Specification Patterns

The following patterns come from the [Specification Pattern Website](http://patterns.projects.cis.ksu.edu/documentation/patterns/ltl.shtml) (for LTL formulae).

#### Absence

P is false :
- Globally:	`[](!P)``
- Before R: `<>R -> (!P U R)`
- After Q: `[](Q -> [](!P))`
- Between Q and R: `[]((Q & !R & <>R) -> (!P U R))`
- (*) After Q until R: `[](Q & !R -> (!P W R))`

#### Existence

P becomes true :
- Globally: `<>(P)`
- (*) Before R: `!R W (P & !R)`
- After Q: `[](!Q) | <>(Q & <>P))`
- (*) Between Q and R: `[](Q & !R -> (!R W (P & !R)))`
- (*) After Q until R: `[](Q & !R -> (!R U (P & !R)))`

#### Bounded Existence

In these mappings, we illustrate one instance of the bounded existence pattern, where the bound is at most 2 designated states. Other bounds can be specified by variations on this mapping.

Transitions to P-states occur at most 2 times:
- Globally: `(!P W (P W (!P W (P W []!P))))`
- Before R:
```
<>R -> ((!P & !R) U (R | ((P & !R) U
         (R | ((!P & !R) U (R | ((P & !R) U
            (R | (!P U R)))))))))
```
- After Q: `<>Q -> (!Q U (Q & (!P W (P W (!P W (P W []!P))))))`
- Between Q and R:
```
[]((Q & <>R) ->
   ((!P & !R) U (R | ((P & !R) U
     (R | ((!P & !R) U (R | ((P & !R) U
       (R | (!P U R))))))))))
```
- After Q until R:
```
[](Q -> ((!P & !R) U (R | ((P & !R) U
          (R | ((!P & !R) U (R | ((P & !R) U
            (R | (!P W R) | []P)))))))))
```
#### Universality

P is true:
- Globally: `[](P)`
- Before R: `<>R -> (P U R)`
- After Q: `[](Q -> [](P))`
- Between Q and R: `[]((Q & !R & <>R) -> (P U R))`
- (*) After Q until R: `[](Q & !R -> (P W R))`

#### Precedence

S precedes P:
- (*) Globally: `!P W S`
- (*) Before R: `<>R -> (!P U (S | R))`
- (*) After Q: `[]!Q | <>(Q & (!P W S))`
- (*) Between Q and R: `[]((Q & !R & <>R) -> (!P U (S | R)))`
- (*) After Q until R: `[](Q & !R -> (!P W (S | R)))`

#### Response

S responds to P :
- Globally: `[](P -> <>S)`
- (*) Before R: `<>R -> (P -> (!R U (S & !R))) U R`
- After Q: `[](Q -> [](P -> <>S))`
- (*) Between Q and R: `[]((Q & !R & <>R) -> (P -> (!R U (S & !R))) U R)`
- (*) After Q until R: `[](Q & !R -> ((P -> (!R U (S & !R))) W R)`

#### Precedence Chain

The following illustrates the 2 cause-1 effect precedence chain.

S, T precedes P:
- Globally: `<>P -> (!P U (S & !P & o(!P U T)))`
- Before R: `<>R -> (!P U (R | (S & !P & o(!P U T))))`
- After Q: `([]!Q) | (!Q U (Q & <>P -> (!P U (S & !P & o(!P U T))))`
- Between Q and R: `[]((Q & <>R) -> (!P U (R | (S & !P & o(!P U T)))))`
- After Q until R: `[](Q -> (<>P -> (!P U (R | (S & !P & o(!P U T))))))`

The following illustrates the 1 cause-2 effect precedence chain.

P precedes (S, T):
- Globally: `(<>(S & o<>T)) -> ((!S) U P))`
- Before R: `<>R -> ((!(S & (!R) & o(!R U (T & !R)))) U (R | P))`
- After Q: `([]!Q) | ((!Q) U (Q & ((<>(S & o<>T)) -> ((!S) U P)))`
- Between Q and R: `[]((Q & <>R) -> ((!(S & (!R) & o(!R U (T & !R)))) U (R | P)))`
- After Q until R: `[](Q -> (!(S & (!R) & o(!R U (T & !R))) U (R | P) | [](!(S & o<>T))))`

#### Response Chain

##### 2 stimulus-1 response chain

P responds to S,T:
- Globally: `[] (S & o<> T -> o(<>(T & <> P)))`
- Before R: `<>R -> (S & o(!R U T) -> o(!R U (T & <> P))) U R`
- After Q: `[] (Q -> [] (S & o<> T -> o(!T U (T & <> P))))`
- Between Q and R: `[] ((Q & <>R) -> (S & o(!R U T) -> o(!R U (T & <> P))) U R)`
- After Q until R:
```
[] (Q -> (S & o(!R U T) -> o(!R U (T & <> P))) U
    (R | [] (S & o(!R U T) -> o(!R U (T & <> P)))))
```

##### 1 stimulus-2 response chain

S,T responds to P:
- Globally: `[] (P -> <>(S & o<>T))`
- Before R: `<>R -> (P -> (!R U (S & !R & o(!R U T)))) U R`
- After Q: `[] (Q -> [] (P -> (S & o<> T)))`
- Between Q and R: `[] ((Q & <>R) -> (P -> (!R U (S & !R & o(!R U T)))) U R)`
- After Q until R
```
[] (Q -> (P -> (!R U (S & !R & o(!R U T)))) U
    (R | [] (P -> (S & o<> T))))
```

#### Constrained Chain Patterns

The following is the 1-2 response chain constrained by a single proposition.

S,T without Z responds to P:
- Globally:	`[] (P -> <>(S & !Z & o(!Z U T)))`
- Before R: `<>R -> (P -> (!R U (S & !R & !Z & o((!R & !Z) U T)))) U R`
- After Q: `[] (Q -> [] (P -> (S & !Z & o(!Z U T))))`
- Between Q and R: `[] ((Q & <>R) -> (P -> (!R U (S & !R & !Z & o((!R & !Z) U T)))) U R)`
- After Q until R:
```
[] (Q -> (P -> (!R U (S & !R & !Z & o((!R & !Z) U T)))) U
    (R | [] (P -> (S & !Z & o(!Z U T)))))
```

## Appendix C: Complete List of Options

The list of all input options to DecentMon can be obtained by executing
```
decentmon.ext --help
```
(where `ext` is either `.native` or `byte` depending on your compilation option)

- `-n`: the maximm number of tests to run to obtain one row benchmark (each test can result in a meaningful sample or not, e.g., in the case of a non-monitorable formula)
- `-sf`: the *size of the formula*
- `-msf`: the *maximum size of the formula* (will test from size 1 to the value provided)
- `-st`: the *size of the trace*
- `-dalpha`: the *decentralized alphabet*
- `-alpha`: the *centralized alphabet* (will consider possible dalphabets generated from it)
- `-abs`: use *abscence* patterns
- `-exis`: use *existence* patterns
- `-bexis`: use *bounded existence* patterns
- `-univ`: use *universality* patterns
- `-prec`: use *precedence* patterns
- `-resp`: use *response* patterns
- `-precc`: use *precedence chain* patterns
- `-respc`: use *response chain* patterns
- `-consc`: use *constrained chain* patterns
- `-prt_trace_mess`: print *trace and number of messages statistics*
- `-prt_delay`: print *delay statistics*
- `-prt_full`: print *full statistics*
- `-flipcoin`: use the *flipcoin* probability distribution (uniform distribution with probabily 0.5)
- `-bernouilli` use the *BERNOUILLI* probability distribution (uniform distribution with a probability given as an argument)
- `-expo`: use the *EXPONENTIAL* probability distribution (the rate parameter is given as an argument)
- `-beta`: use the *BETA* probability distribution (the rate parameters are given as arguments)
- `-only_changes`: if enabled, components send the value of their propositions only if there is a change in its value. More precisely, if among the monitors, there is atleast the value of one proposition that is modified wrt the previous event, the component has to send a message to the monitor
- `-bias`: *bias the generation* of formulae to favor one component; the integer parameter is the index of the component
- `-precision`: the *precision of numbers* (number of decimals)
- `-eval`: CURRENT EVAL
- `-keep_samples`: keep samples
- `-file`: the name of the file on which samples should be stored
- `-nb_samples`: the number of target samples to obtain
- `-help`: display this list of options
- `--help`: display this list of options

We recommend having the value for option `-n` much larger than the value for option `nb_samples`.

## Appendix D: Examples

Examples should be executed at the root of the project (where the decentmon executable is located).

### Benchmark with Random Formulae

By running the following command:
```
./decentmon.native -n 500 -nb_samples 10 -msf 3 -st 1000 -dalpha "{a|b|c}" -prt_full true -keep_samples true -file log.txt
```
it tells DecentMon to execute a benchmark with the following options:
- For each entry line, the maximum number of tests is: 500
- For each entry line, the target number of sample is: 10
- Running benchmarks for formulae of maximum size: 3 (i.e., it will do a complete bench for each formula size between 1 and 3)
- Each formula will be monitored against a freshly randomly generated trace of size: 1000
- The probability distribution that will be used for the trace is flipcoin
- Distributed alphabet used: {a|b|c}
- Full statistics will be displayed.
- Storing the results in `log.txt`

DecentMon will recall the selected options, run for a while and display the results as illustrated by the following screenshot.
One can consult `log.txt to look at the individual sample experiments.

![alt text](doc_images/bench_random_formulae.png)

### Benchmark with Specification Patterns

By running the following command:
```
/decentmon.native -n 500 -nb_samples 10 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 1000 -dalpha "{a|b|c}" -prt_full true -keep_samples true -file log.txt
```
it tells DecentMon to execute a benchmark with the following options:
- For each entry line, the maximum number of tests is: 500
- For each entry line, the target number of sample is: 10
- The following LTL specification pattern(s) will be tested:  abscence  existence  universality  precedence  response  response_chain  precedence_chain  constrained_chain
- Each formula will be monitored against a freshly randomly generated trace of size: 1000
- The probability distribution that will be used for the trace is flipcoin
- Distributed alphabet used: {a|b|c}
- Full statistics will be displayed.
- Storing the results in `log.txt`

DecentMon will recall the selected options, run for a while and display the results as illustrated by the following screenshot.

One can consult `log.txt to look at the individual sample experiments.

![alt text](doc_images/bench_pattern_formulae.png)

### Further Examples

Directory `example_benchs` contains further example.

### Examples from FMSD Papers

Scripts to run the examples from the FMSD paper

> * Christian Colombo, Yliès Falcone:
> Organising LTL monitors over distributed systems with a global clock. Formal Methods in System Design 49(1-2): 109-158 (2016)

are provided within the `example_benchs/fmsd_bench` directory.
