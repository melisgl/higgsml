#!/bin/sh -e
git clone https://github.com/melisgl/mgl.git
(cd mgl && git checkout 7a90a22361a2fbddcf64671065afb32f22c75fd1)

git clone https://github.com/melisgl/mgl-mat
(cd mgl-mat && git checkout 068792aee7f3297635a5a00c49a68334771a2335)

git clone https://github.com/takagi/cl-cuda.git
(cd cl-cuda && git checkout 81e81493bfe8506024a2daf101b3fefa40a9e87a)

git clone https://github.com/takagi/cl-reexport/
(cd cl-reexport && git checkout e49847e14d57a7ce91d82fb9964166f3649587f0)

git clone https://github.com/arielnetworks/cl-pattern/
(cd cl-pattern && git checkout 4717b690d21a3388bec3a4a271d2dcb7cdb561ce)
