#!/bin/sh -e
git clone https://github.com/melisgl/mgl.git
(cd mgl && git checkout 345ba8a8058d96745d6e56a8b35af3c79b2cf70f)

git clone https://github.com/melisgl/mgl-mat
(cd mgl-mat && git 9988b6f246c6970631a0fabd9749c303716b5964)

git clone https://github.com/takagi/cl-cuda.git
(cd cl-cuda && git checkout 77accf98a4944c3130ab159dd17f129ef43709e0)

git clone https://github.com/takagi/cl-reexport/
(cd cl-reexport && git checkout e49847e14d57a7ce91d82fb9964166f3649587f0)

git clone https://github.com/arielnetworks/cl-pattern/
(cd cl-pattern && git checkout 4717b690d21a3388bec3a4a271d2dcb7cdb561ce)
