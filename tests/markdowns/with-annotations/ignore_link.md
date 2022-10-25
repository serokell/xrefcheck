<!--
 - SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

### Ignore the first link in the paragraph

<!-- xrefcheck: ignore link-->
Serokell [web-site](https://serokell.io/)
Serokell [team](https://serokell.io/team)

<!-- xrefcheck: ignore link-->

Serokell [blog](https://serokell.io/blog)

Serokell <!-- xrefcheck: ignore link --> [labs](https://serokell.io/labs)

Serokell <!-- xrefcheck: ignore link -->
[contacts](https://serokell.io/contacts) and again
[team](https://serokell.io/team)

### Ignore not the first link in the paragraph

[team](https://serokell.io/team) again and <!-- xrefcheck: ignore link --> [projects](https://serokell.io/projects)

Also [hire-us](https://serokell.io/hire-us) and <!--xrefcheck: ignore link -->
[fintech](https://serokell.io/fintech-development)
development

Here are [how-we-work](https://serokell.io/how-we-work) and [privacy](https://serokell.io/privacy)
and <!-- xrefcheck: ignore link -->     [ml consulting](https://serokell.io/machine-learning-consulting)

<!-- xrefcheck: ignore link -->
Ignore link bug _regression test_ [link1](link1) [link2](link2)

<!-- xrefcheck: ignore link -->
Another ignore link bug _some [link1](link1) emphasis_ [link2](link2)

### Ignore pragma should be followed by

<!-- xrefcheck: ignore link -->

This annotation expects link in paragraph right after it.

So [link3](link3) is not ignored.

Annotation inside paragraph <!-- xrefcheck: ignore link --> allows
softbreaks and __other *things*__ in paragraph, so [link4](link4) is ignored.
