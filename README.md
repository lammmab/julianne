![Julianne Logo](logo/wide.png)
Julianne language. Interpreted, optionally typed, simplicity focused, and powerful

# NOT COMPLETED!!!! does NOT PRODUCE WORKING BYTECODE NOR IS IT A VIRTUAL MACHINE PLATFORM!

## Building
To build the project, ensure you have:
* Nim version 1.6.0 +
* GCC

1. Clone the repository with:
```bash
git clone https://github.com/lammmab/julianne --recurse-submodules
```
2. CD into the directory with:
```bash
cd julianne
```
3. Build or run the interpreter with:
```bash
nimble -d:nimOldCaseObjects build
```
This will build the interpreter to the specified directory in `julianne.nimble` for usage.
```bash
nimble -d:nimOldCaseObjects run -- FILENAME.jj
```
This will build the interpreter and run it on a specified file.