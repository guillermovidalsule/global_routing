# AVLSI - Global Routing

This projects aims to model a Global Routing algorithm that is state-of-the-art and relevant to the VLSI community.


It uses Fast Lookup Table Based Rectilinear Steiner Minimal Tree (FLUTE), the most state of the art algorithm.

There are two main directories:

+ `geosteiner_routing` : Router that uses GeoSteiner.
+ `flute_routing` : Router that uses FLUTE.

I do not add further information on GeoSteiner since I did not manage to fix the linking in time.
It is the best algorithm when it comes to finding the __ABSOLUTE BEST__ Rectiliniar Steiner Minimal Tree (RSMT).
The original plan was to use this as an _extra_ to compare how it fared against FLUTE.

The complete algorithm that integrates FLUTE performs the following:

+ 1. Each net is interfaced with ANSI C FLUTE.
+ 2. FLUTE is performed on each net.
+ 3. For each branch, all pairs of nodes are routed.
+ 4. Illegal Steiner nodes are repositioned with BFS.
+ 5. For each pair of points, they are routed using A\*, avoiding all obstacles.
+ 6. The path is undone and capacities are updated.

Some Rip-Up was included at first but it increased substantially the complexity and memory, so it was subsequentially removed.

### Prerequisites

You will need the following tools to use the different features of the project:

+ [Alire](https://alire.ada.dev/) (>=2.0.2) for running the SHELL script or manage the crate.
+ [GNAT Ada](https://github.com/alire-project/GNAT-FSF-builds) (>=14.2.0) for compiling the Galton board model.
+ [GPRBUILD](https://github.com/AdaCore/gprbuild) (>=22.0.0) for building the Galton board project.
+ Python3 (>=3.10.12) to run the graph generator (+pip installing the depenencies).

For those of you that are unfamiliar with Ada / Alire, you can follow this quick steps using Alire to set up the environment:

```
# Download Alire (alr) and add it to PATH
wget https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip
unzip alr-2.1.0-bin-x86_64-linux.zip
export $PATH=$PWD/bin

# Install GNAT and GPRBUILD through Alire
alr toolchain --select gnat_native=14.2.1 gprbuild=22.0.1
```

In DockerHUB there are some available containers with Ubuntu22.04 and Alire already set up.

To use the visualization tools you will also need `python3` with `matplotlib` and `pandas`.

For GeoSteiner you need many libraries which are described in the project file, but since it does not work you need not concern yourself with it.

### Compiling

In order to compile an Ada program you can easily do the following:

```
alr build --release
```

The `--release` flag is paramount to compile with optimization flags, otherwise you will compile for DEBUG.

In order to modify the maximum capacity and the accuracy of FLUTE do the following:

```
alr build --release -- -gnateDACC=VALUE1:10 -gnateDCAP=VALUE
```

Please note that the maximum capacity was previously used by RRR until it was removed for performance, so it currently just
serves as a limiting factor.

### Running

Running it, yet again, is fairly simple:

```
./bin/flute_routing path/to/net/file path/to/edge/file
```

You must provide two arguments: the path to the net file and the path to the edge file.

[!CAUTION]
The program must be run from the `flute_routing` directory since FLUTE needs two hardcoded files for the LUT. It will otherwise fail.

### Edge and Net file

The edge file firstly contains four arguments: vertical size, horizontal size, bottom left x-coordinate, bottom left y-coordinate.
Subsequently, you must introduce two matrices with size h x v (what you defined). These matrices indicate the edge capacities
for the horizontal and the vertical ones, respectively. Note that for the horizontal's last column it must all be zeroes, same for the vertical's last row. This has been done for *readability*, else it can be quite confusing. To make obstacles simply block all surrounding edges of a given cell with 0.

The net file is more simple. Just introduce the degree of the network and then the coordinates for as many networks as you want. For example:

```
3
x0 y0 x1 y1 x2 y2
2
x0 y0 x1 y1
```

### Visualization

In order to see an experimental visualization of the results (which are stored in a file named `results.csv` after execution), use the script `visualize.py` in the following manner.

```
python3 visualize.py /path/to/edge/file /path/to/results
```

Please make sure that you use the same edge file as the one you used to produce those results.
