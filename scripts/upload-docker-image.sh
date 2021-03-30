#!/usr/bin/env bash

skopeo --insecure-policy copy --dest-creds "serokell:${DOCKERHUB_PASSWORD}" "$1" "$2"
