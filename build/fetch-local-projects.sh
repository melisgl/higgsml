#!/bin/sh -e
git clone git://github.com/melisgl/mgl.git
(cd mgl && git checkout higgsml)

git clone https://github.com/melisgl/mgl-mat
(cd mgl-mat && git checkout higgsml)

git clone https://github.com/melisgl/cl-cuda
(cd cl-cuda && git checkout higgsml)

git clone https://github.com/takagi/cl-reexport/
(cd cl-reexport && git checkout e49847e14d57a7ce91d82fb9964166f3649587f0)

git clone https://github.com/arielnetworks/cl-pattern/
(cd cl-pattern && git checkout 4717b690d21a3388bec3a4a271d2dcb7cdb561ce)
