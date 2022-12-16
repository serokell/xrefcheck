<!--
 - SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

### Do not check the first link in the paragraph

<!-- xrefcheck: no duplication check in link-->
Serokell [web-site](https://serokell.io/)
Serokell [team](https://serokell.io/team)

<!-- xrefcheck: no duplication check in link-->

Serokell [blog](https://serokell.io/blog)

Serokell <!-- xrefcheck: no duplication check in link --> [labs](https://serokell.io/labs)

Serokell <!-- xrefcheck: no duplication check in link -->
[contacts](https://serokell.io/contacts) and again
[team](https://serokell.io/team)

### Do not check not the first link in the paragraph

[team](https://serokell.io/team) again and <!-- xrefcheck: no duplication check in link --> [projects](https://serokell.io/projects)

Also [hire-us](https://serokell.io/hire-us) and <!--xrefcheck: no duplication check in link -->
[fintech](https://serokell.io/fintech-development)
development

Here are [how-we-work](https://serokell.io/how-we-work) and [privacy](https://serokell.io/privacy)
and <!-- xrefcheck: no duplication check in link -->     [ml consulting](https://serokell.io/machine-learning-consulting)

<!-- xrefcheck: no duplication check in link -->
Do not check link bug _regression test_ [link1](link1) [link2](link2)

<!-- xrefcheck: no duplication check in link -->
Another no duplication check in link bug _some [link1](link1) emphasis_ [link2](link2)

### Do not check pragma should be followed by

<!-- xrefcheck: no duplication check in link -->

This annotation expects link in paragraph right after it.

So [link3](link3) is not checked for copypaste.

Annotation inside paragraph <!-- xrefcheck: no duplication check in link --> allows
softbreaks and __other *things*__ in paragraph, so [link4](link4) is checked for copypaste.
