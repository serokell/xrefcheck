<!--
 - SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: Unlicense
 -->

# Contribution Guidelines

## Reporting issues

Please [open an issue](https://github.com/serokell/crossref-verifier/issues/new/choose) if you find a bug or have a feature request.
Note: you need to login (e. g. using your GitHub account) first.
Before submitting a bug report or feature request, check to make sure it hasn't already been submitted

The more detailed your report is, the faster it can be resolved.
If you report a bug, please provide steps to reproduce this bug and revision of code in which this bug reproduces.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve our project, pull requests are most welcome.

Our pull request template contains a [checklist](./pull_request_template.md#white_check_mark-checklist-for-your-pull-request) of acceptance criteria for your pull request.
Please read it before you start contributing and make sure your contributions adhere to this checklist.

## Legal

We want to make sure that our projects come with correct licensing information
and that this information is machine-readable, thus we are following the
[REUSE Practices][reuse] – feel free to click the link and read about them,
but, basically, it all boils down to the following:

  * Add the following header at the very top (but below the shebang, if there
    is one) of each source file in the repository (yes, each and every source
    file – it is not as hard as it might sound):

    ```haskell
    {- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
     -
     - SPDX-License-Identifier: MPL-2.0
     -}
    ```

    (This is an example for Haskell; adapt it as needed for other languages.)

    The license identifier should be the same as the one in the `LICENSE` file.

  * If you are copying any source files from some other project, and they do not
    contain a header with a copyright and a machine-readable license identifier,
    add it, but be extra careful and make sure that information you are recording
    is correct.

    If the license of the file is different from the one used in the project and
    you do not plan to relicense it, use the appropriate license identifier and
    make sure the license text exists in the `LICENSES` directory.

    If the file contains the entire license in its header, it is best to move the
    text to a separate file in the `LICENSES` directory and leave a reference.

  * If you are copying pieces of code from some other project, leave a note in the
    comments, stating where you copied it from, who is the copyright owner, and
    what license applies.

  * All the same rules apply to documentation that is stored in the repository.

These simple rules should cover most of situation you are likely to encounter.
In case of doubt, consult the [REUSE Practices][reuse] document.

[reuse]: https://reuse.software/spec/
