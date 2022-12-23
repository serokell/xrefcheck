<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

[ First   file  ](./first-file.md)

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
[  fubarw](./nonexistent-file.md)
[  nonexfile](./nonexistent-file.md)
[  fdw](./nonexistent-file.md)


<!-- check external links -->
[github](https://github.com)
[gitlab](https://github.com)
