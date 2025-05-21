## CharacterToUnicodeMap class

A CharacterToUnicodeMap object contains a mapping of single-byte code points to Unicode characters. The single-byte characters are presumably EBCDIC or ASCII, though the class does not explicitly require this - it simply maps code points in some character set to Unicode.

The CharacterToUnicodeMap class provides programmers with a facility for creating or copying codepages that are not currently available in the current Sirius Mods version. Besides methods for creating and modifying your own codepage, the class also has methods for dynamically invoking one of the Sirius Mods codepages supported at your site. You can use the String class CharacterToUnicode function which returns the mapped Unicode characters for its string method object.

The CharacterToUnicodeMap class is new as of version 8.0 of the Sirius Mods.

### Example

The following request generates a basic ASCII to Unicode codepage, then converts the ASCII '01234' equivalent hex characters to Unicode. If you attempt to extract a translation of a code point not in the mapping, say an ASCII code point with the high order bit on like X'80', you get a Character TranslationException exception:

```
%i is float
%1 is longstring
%u is unicode
%tr is object characterToUnicodeMap
begin
for %i from 0 to 127
    %1 = %1 with %i: integerToBinary(1)
    %u = %u: unicodeWith(%i:integerToBinary(2):utf16ToUnicode)
end for
%tr = new(in=%1, out=%u)
printText {'3031323334':x:characterToUnicode(%tr)}
end
```

The output of the above request is:

01234

### Further examples

See the NewFromEbcdicCodepage examples.

### List of CharacterToUnicodeMap methods

The List of CharacterToUnicodeMap methods shows all the class methods.

Category: System classes
