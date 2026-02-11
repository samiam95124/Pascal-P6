# Debugging Pascal-P6 Runtime Errors Without Line Numbers

## Problem
When pgen (the P6 code generator) produces a runtime error like "Value out of range", it doesn't include the module name and line number where the error occurred. This makes debugging difficult.

## Solution: Use GDB to Catch Runtime Errors

### Method 1: GDB Breakpoint on Error Functions

The Pascal-P6 runtime library (psystem.c) has error handling functions that we can set breakpoints on.

#### Step 1: Start GDB with the program and arguments

```bash
gdb p2c sample_programs/prime
```

#### Step 2: Set breakpoints on error functions

```gdb
(gdb) b errorv
Breakpoint 1 at 0x4f37ec: file psystem.c, line 549.

(gdb) b errore
Breakpoint 2 at 0x4f40c3: file psystem.c, line 689.
```

**Key functions:**
- `errorv` - Error with value
- `errore` - Error with exception

#### Step 3: Run the program with arguments

```gdb
(gdb) r sample_programs/prime
```

The program will run until it hits the error, then break:

```
Breakpoint 2, errore (modnam=0x1 <error: Cannot access memory at address 0x1>, line=0, en=1)
    at psystem.c:689
```

#### Step 4: Get the backtrace to find the source line

```gdb
(gdb) bt
#0  errore (...) at psystem.c:689
#1  0x00000000004f6c78 in psystem_errore (modnam=0x4e22a6 <modnam> "p2c", line=3707, en=0)
    at psystem.c:2023
#2  0x000000000044d296 in p2c () at /home/.../utils/p2c.pas:3707
#3  0x0000000000000000 in ?? ()
```

**The key information is in frame #2:** Shows the exact source file and line number where the error occurred!

In this example:
- File: `utils/p2c.pas`
- Line: `3707`
- Function: `p2c()`

### What the Error Was

Line 3707 was accessing `p^.text[p^.len]` where `p^.len > 8000`, but the array is only `[1..8000]`, causing a range check violation.

```pascal
while (p^.len > 0) and ((p^.text[p^.len] = ' ') or (p^.text[p^.len] = chr(9))
       or (p^.text[p^.len] = chr(10))) do  (* <-- Line 3707 *)
  p^.len := p^.len - 1;
```

### Method 2: Assembly-Level Debugging (Advanced)

If GDB doesn't provide enough information, you can examine the generated `.s` assembly files to understand the exact instruction that failed.

## Tips

1. **Build with debug symbols**: The P6 compiler includes debug information by default
2. **Use relative paths**: If GDB shows path errors, the source location in frame #2 is still accurate
3. **Check array bounds**: Most "Value out of range" errors are array index or subrange violations
4. **Look for subrange types**: Variables declared as `var x: 1..100` will cause range errors if assigned values outside that range

## Common Runtime Errors

| Error | Likely Cause |
|-------|--------------|
| Value out of range | Array index out of bounds, or subrange variable assignment |
| Division by zero | Divide or mod by zero |
| Invalid case value | Case statement with value not covered |
| Nil pointer dereference | Accessing through an uninitialized or disposed pointer |

## Future Improvements

The pgen code generator should be enhanced to include module and line number information in runtime error messages, similar to what modern compilers provide. This would eliminate the need for GDB in most debugging scenarios.
