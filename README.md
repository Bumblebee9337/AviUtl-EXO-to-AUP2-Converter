# AviUtl-EXO-to-AUP2-Converter
A tool to migrate legacy 32-bit AviUtl .exo files into modern 64-bit .aup2 files for AviUtl2/ExEdit2.

# Synopsis

A standalone parsing engine written in **FreeBASIC** designed to migrate legacy 32-bit AviUtl project timelines (`.exo`) into the modern 64-bit AviUtl2 ecosystem (`.aup2`).

## 🚀 Key Features

* **Migration Automation:** Successfully translates foundational media objects, basic shapes, trackbar parameters, and core filters.
* **Scene Reordering & Linking:** Dynamically parses native legacy `.aup` files to reconstruct multi-scene hierarchies, avoiding the limitation where scene object linkages are lost upon import.
* **Audio-Signature FPS Filtering:** Automatically isolates audio tracks (such as 44.1kHz AAC) to prevent timeline framerate desynchronization errors during ingestion.
* **Environment Alignment:** Tailored for `.exo` files generated under traditional English interface patches, outputting text configurations compatible with AviUtl2 using the *Community Translation Companion* plugin.

## 🛠️ Usage Instructions

1. Download `exo2aup2.exe` and `aup2.map` into a directory of your choice.
2. Place the source `.exo` files into the directory. To recreate the project itself, include the parent project `.aup` file.
3. Run the utility to output individual and combined `.aup2` files.
4. Load the resulting `.aup2` file directly into the AviUtl2/ExEdit2 timeline workspace.

*Note: Unmapped objects or complex effects not represented in the mapping file will be logged as warnings, allowing AviUtl2 to ingest the file without structural crashes.*
If your legacy .aup project is using default scene names (i.e Root, Scene 1, Scene 2, etc.) rename them before exporting to the exo format.

Hopefully this will save you some time & effort.

## ⚖️ License

Distributed under the **MIT License**. You are free to modify, distribute, and integrate this parsing logic into your own workflow tools provided credit to the original authorship is maintained.
