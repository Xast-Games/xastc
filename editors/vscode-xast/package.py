#!/usr/bin/env python3
"""Packs the extension into a .vsix (a zip with two manifests), without npm."""
import json
import zipfile
from pathlib import Path

ROOT = Path(__file__).parent
FILES = ["package.json", "extension.js", "language-configuration.json", "README.md", "syntaxes/xast.tmLanguage.json"]

CONTENT_TYPES = """<?xml version="1.0" encoding="utf-8"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension=".json" ContentType="application/json"/>
  <Default Extension=".md" ContentType="text/markdown"/>
  <Default Extension=".vsixmanifest" ContentType="text/xml"/>
</Types>
"""

MANIFEST = """<?xml version="1.0" encoding="utf-8"?>
<PackageManifest Version="2.0.0" xmlns="http://schemas.microsoft.com/developer/vsx-schema/2011">
  <Metadata>
    <Identity Language="en-US" Id="{name}" Version="{version}" Publisher="{publisher}"/>
    <DisplayName>{displayName}</DisplayName>
    <Description xml:space="preserve">{description}</Description>
    <Categories>Programming Languages</Categories>
    <Properties>
      <Property Id="Microsoft.VisualStudio.Code.Engine" Value="{engine}"/>
    </Properties>
  </Metadata>
  <Installation>
    <InstallationTarget Id="Microsoft.VisualStudio.Code"/>
  </Installation>
  <Dependencies/>
  <Assets>
    <Asset Type="Microsoft.VisualStudio.Code.Manifest" Path="extension/package.json" Addressable="true"/>
  </Assets>
</PackageManifest>
"""

def main() -> None:
    package = json.loads((ROOT / "package.json").read_text())
    manifest = MANIFEST.format(engine=package["engines"]["vscode"], **package)
    output = ROOT / "xast-syntax.vsix"
    with zipfile.ZipFile(output, "w", zipfile.ZIP_DEFLATED) as vsix:
        vsix.writestr("[Content_Types].xml", CONTENT_TYPES)
        vsix.writestr("extension.vsixmanifest", manifest)
        for file in FILES:
            vsix.write(ROOT / file, f"extension/{file}")
    print(f"Packed {output.name}")

if __name__ == "__main__":
    main()
