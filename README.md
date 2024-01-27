## Controls

- Arrow keys to move
- Esc to quit

## Runtime Dependencies

- Short answer:
  - OpenGL
  - X11
  - gmp
- Long answer (ldd output):
  - linux-vdso.so.1
  - libm.so.6 => /usr/lib/libm.so.6
  - libGLU.so.1 => /usr/lib/libGLU.so.1
  - libGL.so.1 => /usr/lib/libGL.so.1
  - libgmp.so.10 => /usr/lib/libgmp.so.10
  - libc.so.6 => /usr/lib/libc.so.6
  - /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2
  - libOpenGL.so.0 => /usr/lib/libOpenGL.so.0
  - libstdc++.so.6 => /usr/lib/libstdc++.so.6
  - libgcc_s.so.1 => /usr/lib/libgcc_s.so.1
  - libGLdispatch.so.0 => /usr/lib/libGLdispatch.so.0
  - libGLX.so.0 => /usr/lib/libGLX.so.0
  - libX11.so.6 => /usr/lib/libX11.so.6
  - libxcb.so.1 => /usr/lib/libxcb.so.1
  - libXau.so.6 => /usr/lib/libXau.so.6
  - libXdmcp.so.6 => /usr/lib/libXdmcp.so.6

## Build release from source

- `cabal build --constraint="apecs-physics +release"`