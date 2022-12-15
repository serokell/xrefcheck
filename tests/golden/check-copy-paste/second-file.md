<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

<!-- this file should throw 4 scan errors and
     report 4 possible copypastes (3 local and 1 external) -->

[ First   file  ](./first-file.md)

<!-- This link is not checked for copypaste -->
<!-- xrefcheck: no duplication check in link -->
[   Link 2](./first-file.md)

<!-- This one is not reported because anchor is not the same -->
[   Link 3](./first-file.md#heading)

<!-- And these ones are checked and reported -->
[   Lol Kek](./first-file.md)
[   Baz quux](./first-file.md)

<!-- These ones are not reported because none of link -->
<!-- names is a subsequence of a link                 -->
[  asd](./first-file.md#anch)
[  fdw](./first-file.md#anch)

<!-- These ones are reported because -->
<!-- ff-cho is a subsequence of link+anchor -->
[  ff-cho](./first-file.md#chor)
[  fdw](./first-file.md#chor)


<!-- emit error: unrecognized annotation -->
<!-- xrefcheck: no dh -->

<!-- this emits an error because of expected -->
<!-- paragraph after the annotation -->

<!-- xrefcheck: no duplication check in paragraph -->

# asd

<!-- this emits an error because of expected -->
<!-- link after the annotation               -->
<!-- xrefcheck: no duplication check in link -->

# asd

<!-- this emits an error -->
<!-- xrefcheck: no duplication check in file -->

<!-- Some different pragmas within one link are OK -->
<!-- xrefcheck: ignore link -->
<!-- xrefcheck: no duplication check in link -->
[   Link 3](./first-file.md)

<!-- Some different pragmas within one paragraph are OK -->
<!-- xrefcheck: no duplication check in paragraph -->
<!-- xrefcheck: ignore paragraph -->
hello, how are you, bye

<!-- this paragraph is totally ignored and hence not checked -->
<!-- xrefcheck: ignore paragraph -->
[github](https://github.com)
[gitlab](https://github.com)

<!-- check external links -->
[github](https://github.com)
[gitlab](https://github.com)

<!-- here links are verified, but not checked for copypaste -->
<!-- xrefcheck: no duplication check in paragraph -->
[github](https://github.com)
[gitlab](https://github.com)
