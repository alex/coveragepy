#!/bin/sh

watch-path 'clear; find coverage/ -iname "*.so" -delete; cargo fmt --manifest-path coverage/rstracer/Cargo.toml && cargo check --manifest-path coverage/rstracer/Cargo.toml && python setup.py develop && python igor.py test_with_tracer c -x -v'
