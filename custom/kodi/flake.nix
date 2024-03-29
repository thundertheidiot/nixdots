{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    systems.url = "github:nix-systems/x86_64-linux";

    kodi = {
      url = "github:xbmc/xbmc/4b95737efaddf7c869736e539318f18433413ff1"; # 20.5
      flake = false;
    };
    libdvdcss = {
      url = "github:xbmc/libdvdcss/1.4.3-Next-Nexus-Alpha2-2";
      flake = false;
    };
    libdvdnav = {
      url = "github:xbmc/libdvdnav/6.1.1-Next-Nexus-Alpha2-2";
      flake = false;
    };
    libdvdread = {
      url = "github:xbmc/libdvdread/6.1.3-Next-Nexus-Alpha2-2";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }@inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      lib = pkgs.lib;
      stdenv = pkgs.stdenv;
      kodi = with pkgs;
        stdenv.mkDerivation {
          pname = "kodi";
          version = "20.5";

          src = inputs.kodi;
          buildInputs = [
            gnutls
            libidn2
            libtasn1
            nasm
            p11-kit
            libxml2
            python3
            boost
            libmicrohttpd
            gettext
            pcre-cpp
            yajl
            fribidi
            libva
            libdrm
            openssl
            gperf
            tinyxml2
            taglib
            libssh
            gtest
            ncurses
            spdlog
            alsa-lib
            libGL
            libGLU
            fontconfig
            freetype
            ftgl
            libjpeg
            libpng
            libtiff
            libmpeg2
            libsamplerate
            libmad
            libogg
            libvorbis
            flac
            libxslt
            systemd
            lzo
            libcdio
            libmodplug
            libass
            libbluray
            libudfread
            sqlite
            libmysqlclient
            avahi
            lame
            curl
            bzip2
            zip
            unzip
            glxinfo
            libcec
            libcec_platform
            dcadec
            libuuid
            libxcrypt
            libgcrypt
            libgpg-error
            libunistring
            libcrossguid
            libplist
            bluez
            giflib
            glib
            harfbuzz
            lcms2
            xorg.libpthreadstubs
            ffmpeg
            flatbuffers
            fstrcmp
            rapidjson
            lirc
            mesa # for libEGL
            dbus
            pipewire
            samba
            udev
            wayland
            waylandpp.dev
            wayland-protocols
            libxkbcommon.dev
            libnfs
            xorg.libX11
            xorg.xorgproto
            xorg.libXt
            xorg.libXmu
            xorg.libXext.dev
            xorg.libXdmcp
            xorg.libXinerama
            xorg.libXrandr.dev
            xorg.libXtst
            xorg.libXfixes
            rtmpdump
            libinput.dev
          ];

          nativeBuildInputs = [
            cmake
            doxygen
            makeWrapper
            which
            pkg-config
            autoconf
            automake
            libtool
            jre_headless
            yasm
            gettext
            python3
            flatbuffers
          ];

          depsBuildBuild = [
            buildPackages.stdenv.cc
          ];

          cmakeFlags = [
            "-DAPP_RENDER_SYSTEM=gl"
            "-Dlibdvdcss_URL=${inputs.libdvdcss}"
            "-Dlibdvdnav_URL=${inputs.libdvdnav}"
            "-Dlibdvdread_URL=${inputs.libdvdread}"
            "-DGIT_VERSION=20240302"
            "-DENABLE_EVENTCLIENTS=ON"
            "-DENABLE_INTERNAL_CROSSGUID=OFF"
            "-DENABLE_INTERNAL_RapidJSON=OFF"
            "-DENABLE_OPTICAL=ON"
            "-DLIRC_DEVICE=/run/lirc/lircd"
            "-DENABLE_INTERNAL_FFMPEG=ON"
            "-DSWIG_EXECUTABLE=${buildPackages.swig}/bin/swig"
            "-DFLATBUFFERS_FLATC_EXECUTABLE=${buildPackages.flatbuffers}/bin/flatc"
            "-DPYTHON_EXECUTABLE=${buildPackages.python3Packages.python}/bin/python"
            # When wrapped KODI_HOME will likely contain symlinks to static assets
            # that Kodi's built in webserver will cautiously refuse to serve up
            # (because their realpaths are outside of KODI_HOME and the other
            # whitelisted directories). This adds the entire nix store to the Kodi
            # webserver whitelist to avoid this problem.
            "-DKODI_WEBSERVER_EXTRA_WHITELIST=${builtins.storeDir}"
            "-DWAYLANDPP_SCANNER=${buildPackages.waylandpp}/bin/wayland-scanner++"
          ];
        };

      preConfigure =
        ''
          cmakeFlagsArray+=("-DCORE_PLATFORM_NAME=${lib.concatStringsSep " " ["wayland" "x11"]}")
        ''
        + lib.optionalString (stdenv.hostPlatform != stdenv.buildPlatform) ''
          # Need these tools on the build system when cross compiling,
          # hacky, but have found no other way.
          CXX=$CXX_FOR_BUILD LD=ld make -C tools/depends/native/JsonSchemaBuilder
          cmakeFlags+=" -DWITH_JSONSCHEMABUILDER=$PWD/tools/depends/native/JsonSchemaBuilder/bin"

          CXX=$CXX_FOR_BUILD LD=ld make EXTRA_CONFIGURE= -C tools/depends/native/TexturePacker
          cmakeFlags+=" -DWITH_TEXTUREPACKER=$PWD/tools/depends/native/TexturePacker/bin"
        '';

      postPatch = ''
        substituteInPlace xbmc/platform/posix/PosixTimezone.cpp \
          --replace 'usr/share/zoneinfo' 'etc/zoneinfo'
      '';

      postInstall = ''
        # TODO: figure out which binaries should be wrapped this way and which shouldn't
        for p in $(ls --ignore=kodi-send $out/bin/) ; do
          wrapProgram $out/bin/$p \
            --prefix PATH ":" "${lib.makeBinPath ([pkgs.python3 pkgs.glxinfo pkgs.xdpyinfo pkgs.samba])}" \
            --prefix LD_LIBRARY_PATH ":" "${lib.makeLibraryPath
          (with pkgs; [curl systemd libmad libvdpau libcec libcec_platform libass libnfs rtmpdump])}"
        done

        wrapProgram $out/bin/kodi-send \
          --prefix PYTHONPATH : $out/${pkgs.python3.sitePackages}

        substituteInPlace $out/share/xsessions/kodi.desktop \
          --replace kodi-standalone $out/bin/kodi-standalone
      '';

      doInstallCheck = true;

      installCheckPhase = "$out/bin/kodi --version";

      passthru = {
        pythonPackages = pkgs.python3;
      };
    in {
      defaultPackage = kodi;
      defaultApp = flake-utils.lib.mkApp {
        drv = kodi;
      };
    });
}
