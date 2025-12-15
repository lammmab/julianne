![Julianne Logo](logo/wide.png)
Julianne language. Interpreted, optionally typed, simplicity focused, and powerful.

The coming syntax:
```rust
const PI = 3.14;

class Vector2 {
    x :: float;
    y :: float;
    fn new(a,b) {
        self.x = a
        self.y = b
    }

    fn op+(b) {
        return self:new(self.x+b.x,self.y+b.y)
    }
}

let my_position = Vector2:new(6,7);
let their_position = Vector2:new(4,1);
let new_position = my_position + their_position;
print(f"x: {new_position.x}, y: {new_position.y}") # prints x: 10, y: 8
```

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
nimble build
```
This will build the interpreter to the specified directory in `julianne.nimble` for usage.
```bash
nimble run -- FILENAME.jj
```
This will build the interpreter and run it on a specified file.