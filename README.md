# portio
---

`portio` is another example of me saying "I wonder if I can get this done in
haskell."  In my efforts to learn the language I look for any excuse to use
it.

`portio` is for manipulating bits of memory mapped IO.  It takes 3 arguments:
an address containing the bit to change or read, the index of the bit itself
(0 based), and the value to set that bit to.

I had the need to read and write GPIO ports on a PC designed for use in
vehicles.  These units have custom IO ports starting at address 0xee0.  So, for
example, to test if ignition is turned on or not, one may read 0xee0 and pay
attention to the value for bit 2 (the third bit):

`portio -a 0xee0 -b2 -v off`

Note that in this example, the bit in question is read only so setting to 
something has no effect.  `portio` will just output the value that it read from
the port.  It will indicate that it is writing a byte with the specified bit
cleared. It can't modifiy a read-only bit though. 

## Command line args

```
portio -a <address> -b <bit number> -v <on|off>
  -a   --address=  The address to read or write to
  -b   --bit=      The bit to write
  -v   --value=    The value to write
  -V   --version   Print version information
  -h   --help      Print this usage information
```

To set a bit, `value` may be `true`, `yes`, `on`, or `high`.  Any other value
for the `value` parameter will clear the bit.

## Compiling

```
ghc -o portio Main.hs options.hs --make
```

I really must start using `cabal` for this sort of thing.

### Complications

`portio` makes use of `System.Posix.IO.ByteString`, however there are two
modules with this name: one is in the package `unix`, and the other is in the
package `unix-bytestring`.  In order to indicate which
`System.Posix.IO.ByteString` I wanted, I had to `cabal install unix-bytestring`
and put the following in my code:

    {-# LANGUAGE PackageImports #-}

    module Main where

    import qualified "unix-bytestring" System.Posix.IO.ByteString as BSIO
