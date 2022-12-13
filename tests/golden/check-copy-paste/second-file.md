<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

[ First   file  ](./first-file.md)

<!-- xrefcheck: no duplication check in link -->
[   Link 2](./first-file.md)

<!-- xrefcheck: no dh -->

<!-- this emits an error -->
<!-- xrefcheck: no duplication check in paragraph -->

# asd

<!-- this emits an error -->
<!-- xrefcheck: no duplication check in link -->

# asd

<!-- this emits an error -->
<!-- xrefcheck: no duplication check in file -->

<!-- Some different pragmas within one link is OK -->
<!-- xrefcheck: ignore link -->
<!-- xrefcheck: no duplication check in link -->
[   Link 3](./first-file.md)


<!-- This link is not checked for copypaste -->
<!-- xrefcheck: no duplication check in link -->
[   Link 4](./first-file.md)

<!-- And this one is checked and reported -->
[   Link 5](./first-file.md)
[   Lol Kek](./first-file.md)


<!-- check external links -->
[github](https://github.com)
[gitlab](https://github.com)

<!-- here links are verified, but not checked for copypaste -->
<!-- xrefcheck: no duplication check in paragraph -->
[github](https://github.com)
[gitlab](https://github.com)

<!-- this paragraph is totally ignored -->
<!-- xrefcheck: no duplication check in paragraph -->
<!-- xrefcheck: ignore paragraph -->
[github](https://github.com)
[gitlab](https://github.com)
