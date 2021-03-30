#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

skopeo --insecure-policy copy --dest-creds "serokell:${DOCKERHUB_PASSWORD}" "$1" "$2"
