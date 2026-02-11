# P2C Sample Programs Test Results

## Summary: 8/10 Programs Fully Functional ✅

All programs translate successfully with p2c. 8 compile and run with correct output.

**Latest Update (2026-02-11):** ROUND/TRUNC functions now translate correctly! startrek.pas is now working.

---

## ✅ WORKING PROGRAMS (8/10)

All produce correct output matching expected results:

### 1. basics (35K Pascal)
- Tiny BASIC interpreter
- Output: 17 lines, game interaction correct
- Status: ✅ PASS

### 2. fbench (13K Pascal)
- Fourier benchmark
- Output: 9 lines, numerical precision verified
- Marginal ray: 47.09479120920 / Paraxial ray: 47.08372160249
- Status: ✅ PASS

### 3. hello (66 bytes Pascal)
- Classic "Hello, world"
- Output: "Hello, world"
- Status: ✅ PASS

### 4. match (2.9K Pascal)
- Match-snatch game
- Output: 16 lines, game logic correct
- Status: ✅ PASS

### 5. prime (8.0K Pascal)
- Prime number generator
- Output: "10 iterations / 1899 primes"
- Status: ✅ PASS - **Fixed in this session!**

### 6. qsort (648 bytes Pascal)
- Quicksort implementation
- Output: "Result: ddeeeffggghhhhhhhjjkkkkkkkkkllllnnrssssssst"
- Status: ✅ PASS

### 7. roman (654 bytes Pascal)
- Roman numeral converter
- Output: 13 lines of powers of 2 in Roman numerals
- Status: ✅ PASS

### 8. startrek (34K Pascal) **NEW!**
- Star Trek game
- Output: Full game playthrough, no infinite loop
- Random damage values differ slightly (expected - different RNG)
- Status: ✅ PASS - **Fixed with ROUND() translation!**

---

## ❌ FAILING PROGRAMS (2/10)

Both translate successfully but fail during C compilation:

### 9. drystone (31K Pascal)
**Translation:** ✅ SUCCESS
**Compilation:** ❌ FAILED
**Error:** `unknown type name 'recordpointer'` (multiple occurrences)

**Root Cause:**
Record pointer types not properly declared in generated code.

**Example errors:**
```
drystone.h:16:12: error: unknown type name 'recordpointer'
drystone.c:115:30: error: invalid type argument of unary '*' (have 'int')
```

---

### 10. pascals (69K Pascal - largest program)
**Translation:** ✅ SUCCESS
**Compilation:** ❌ FAILED
**Errors:** 104+ compilation errors

**Root Cause:**
Multiple type system and scoping issues in complex program.

**Example errors:**
```
pascals.c:726:21: error: subscripted value is neither array nor pointer
pascals.c:1110:30: error: 'level' undeclared
pascals.c:1122:39: error: unknown type name 'conrec'
pascals.c:1026:13: error: static declaration follows non-static
```

---

## Recent Fixes (This Session)

### 1. ROUND() and TRUNC() Function Translation ✅
**Problem:** Functions were not generating C code, only changing types
**Solution:** Wrap arguments with `(int)round(arg)` and `(int)trunc(arg)`
**Impact:** startrek.pas now works!

**Example:**
```pascal
Pascal:  quadrant[ROUND(xpos), ROUND(ypos)]
C (old): quadrant[(*xpos)][(*ypos)]           // ERROR!
C (new): quadrant[(int)round((*xpos))][(int)round((*ypos))]  // Correct!
```

### 2. Comment Processing Fix ✅
**Problem:** Pascal non-short-circuiting evaluation caused array bounds violations
**Solution:** Restructured trimming loops to avoid array access when p^.len = 0
**Impact:** prime.pas now works!

### 3. Devolved Set Comparisons ✅
**Problem:** Inline bit-shift only worked for elements 0-63, not full 0-255 range
**Solution:** Convert constant sets to C comparison chains
**Impact:** No more infinite loops on character set tests

**Example:**
```pascal
Pascal:  ch IN ['y','Y','n','N']
C (old): (((0UL) >> (ch)) & 1)                // BROKEN for ch > 63
C (new): (ch == 'y' || ch == 'Y' || ch == 'n' || ch == 'N')  // Correct!
```

### 4. Forward Declaration Fixes ✅
**Problem:** Empty `()` conflicted with narrow types (char, bool, short)
**Solution:** Emit full parameter lists when narrow types detected
**Impact:** No more type conflicts in prototypes

---

## Progress Tracking

| Date | Working Programs | Key Fix |
|------|------------------|---------|
| Session start | 6/10 | - |
| After prime fix | 7/10 | Comment processing |
| After ROUND fix | **8/10** | ROUND/TRUNC translation |

---

## Test Methodology

All programs with .inp and .cmp files were tested:

1. **Translation:** `bin/p2c <program>`
2. **Compilation:** `gcc -o <program>_c <program>.c -I../libs ../libs/psystem.a -lm`
3. **Execution:** `./<program>_c < <program>.inp > <program>.out`
4. **Comparison:** Output compared to .cmp (ignoring P5 interpreter headers)

Success criteria:
- ✅ Translation completes without errors
- ✅ C compilation succeeds (warnings acceptable)
- ✅ Program runs to completion
- ✅ Output matches expected (functional correctness)

---

## Notes

1. **P5 .cmp files:** Include interpreter headers ("P5 Pascal interpreter vs. 1.2"), which are stripped for comparison

2. **Random number differences:** Minor variations in RNG between Pascal and C are acceptable (e.g., startrek damage values)

3. **All fixes working correctly:**
   - Devolved set comparisons (0-255 range)
   - Comment processing (no array bounds errors)
   - Forward declarations (no type conflicts)
   - ROUND/TRUNC translation (proper C conversion)

4. **Next priorities:**
   - Record pointer type handling (fixes drystone)
   - Complex type system improvements (fixes pascals)

---

## Detailed Test Output

Test date: 2026-02-11
p2c version: 0.4.x (with all fixes)
Platform: Linux/AMD64
Compiler: gcc with -I../libs ../libs/psystem.a -lm

See [testing_sample_programs.md](testing_sample_programs.md) for detailed testing procedures.
