### Info


### Usage

To download the Source Sans Pro TTF font directly, pull the file from the official Adobe Fonts Source Sans GitHub Repository:

```sh
pushd fonts/SourceSansPro
curl -skLo SourceSansPro-Black.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Black.ttf
curl -skLo SourceSansPro-BlackItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-BlackIt.ttf
curl -skLo SourceSansPro-Bold.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Bold.ttf
curl -skLo SourceSansPro-BoldItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-BoldIt.ttf
curl -skLo SourceSansPro-ExtraLight.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-ExtraLight.ttf
curl -skLo SourceSansPro-ExtraLightItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-ExtraLightIt.ttf
curl -skLo SourceSansPro-Italic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-It.ttf
curl -skLo SourceSansPro-Light.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Light.ttf
curl -skLo SourceSansPro-LightItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-LightIt.ttf
curl -skLo SourceSansPro-Regular.ttf  https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Regular.ttf
curl -skLo SourceSansPro-SemiBold.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Semibold.ttf
curl -skLo SourceSansPro-SemiBoldItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-SemiboldIt.ttf
popd
```

```sh
pushd fonts/SourceCodePro
BASE_URL=https://github.com/adobe-fonts/source-code-pro/tree/release/TTF
curl -skLo SourceCodePro-Black.ttf $BASE_URL/SourceCodePro-Black.ttf
curl -skLo SourceCodePro-BlackItalic.ttf $BASE_URL/SourceCodePro-BlackIt.ttf
curl -skLo SourceCodePro-Bold.ttf $BASE_URL/SourceCodePro-Bold.ttf
curl -skLo SourceCodePro-BoldItalic.ttf $BASE_URL/SourceCodePro-BoldIt.ttf
curl -skLo SourceCodePro-ExtraLight.ttf $BASE_URL/SourceCodePro-ExtraLight.ttf
curl -skLo SourceCodePro-ExtraLightItalic.ttf $BASE_URL/SourceCodePro-ExtraLightIt.ttf
curl -skLo SourceCodePro-Italic.ttf $BASE_URL/SourceCodePro-It.ttf
curl -skLo SourceCodePro-Light.ttf $BASE_URL/SourceCodePro-Light.ttf
curl -skLo SourceCodePro-LightItalic.ttf $BASE_URL/SourceCodePro-LightIt.ttf
curl -skLo SourceCodePro-Medium.ttf $BASE_URL/SourceCodePro-Medium.ttf
curl -skLo SourceCodePro-MediumItalic.ttf $BASE_URL/SourceCodePro-MediumIt.ttf
curl -skLo SourceCodePro-Regular.ttf $BASE_URL/SourceCodePro-Regular.ttf
curl -skLo SourceCodePro-SemiBold.ttf $BASE_URL/SourceCodePro-Semibold.ttf
curl -skLo SourceCodePro-SemiBoldItalic.ttf $BASE_URL/SourceCodePro-SemiboldIt.ttf
```

```bash
xvfb-run lualatex --shell-escape main.tex
```

To cleanup temp files use:
```bash
latexmk -c
```

> Note: in order for the table of contents to be generated correctly, you must run build command twice. 
