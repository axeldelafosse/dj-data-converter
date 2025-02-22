# dj-data-converter

A command-line app for converting data files to and from [Traktor](https://www.native-instruments.com/en/products/traktor/dj-software/traktor-pro-3/) and [Rekordbox](https://rekordbox.com/en/).

> This is a fork of [digital-dj-tools/dj-data-converter](https://github.com/digital-dj-tools/dj-data-converter) with some additional features.
> This version converts the key, the label and the color of the tracks. If you want to use it, you need to compile and package the app yourself.

## Features

| Feature                                                      | ✅  |
| ------------------------------------------------------------ | --- |
| Convert from Traktor to Rekordbox                            | Yes |
| Convert from Rekordbox to Traktor                            | Yes |
| Convert tempo (BPM) and beat grid                            | Yes |
| Convert cue points and loops                                 | Yes |
| Convert multiple beat grid markers                           | Yes |
| Convert key                                                  | Yes |
| Convert color                                                | Yes |
| Correct 26ms grid offset when converting mp3 files           | Yes |
| Convert unsupported Traktor cue types using a colour mapping | Yes |
| Convert playlists                                            | No  |
| Runs on Windows                                              | Yes |
| Runs on Mac                                                  | Yes |

## Usage

### Windows

1. Open a command prompt and change to the directory where the archive was extracted:
   ```
   cd <download-dir>
   ```
2. Now execute the app, providing the location of the Traktor collection file, or an exported Rekordbox collection file:

   ```
   dj-data-converter-win.exe [options] <traktor-or-rekordbox-collection-file>
   ```

   For example, assuming Traktor is installed in the default location on Windows:

   ```
   dj-data-converter-win.exe "C:\Users\<your-user-name>\Documents\Native Instruments\Traktor <version-number>\collection.nml"
   ```

   A converted `rekordbox.xml` or `collection.nml` file will be created in the current directory.

   If the conversion fails due to an error, an `error-report.edn` file will be created, also in the current directory.

### Mac

1. Open a terminal and change to the directory where the archive was extracted:
   ```
   cd <download-dir>
   ```
2. Now execute the app, providing the location of the Traktor collection file, or an exported Rekordbox collection file:

   ```
   ./dj-data-converter-macos [options] <traktor-or-rekordbox-collection-file>
   ```

   For example, assuming Traktor is installed in the default location on Mac OS:

   ```
   ./dj-data-converter-macos "/Users/<your-user-name>/Documents/Native Instruments/Traktor <version-number>/collection.nml"
   ```

   A converted `rekordbox.xml` or `collection.nml` file will be created in the current directory.

   If the conversion fails due to an error, an `error-report.edn` file will be created, also in the current directory.

### Options

```
  -h, --help
```

### Importing to Rekordbox

- In the Rekordbox preferences:
  - Go to `View` and under `Layout` enable `rekordbox xml`
  - Go to `Advanced` `Database` and under `rekordbox xml` change `Imported Library` to the location of the generated `rekordbox.xml` file
- The `rekordbox xml` entry should now be visible in the tree lower-left.
- Click the refresh icon to load the file.
- Expand the `rekordbox xml` icon in the tree and select the `All Tracks` entry
- The converted tracks should now be listed in the track list.
- Right-click and select `Import to Collection` as normal.
- Now load the converted tracks and check the converted data. Report any problems as GitHub issues in this project.

### Importing to Traktor

- Back up any existing `collection.nml` file. The location of this file will depend on whether the app is being used on Windows or Mac:

  | OS      | File Location                                                                                    |
  | ------- | ------------------------------------------------------------------------------------------------ |
  | Windows | `C:\Users\<your-user-name>\Documents\Native Instruments\Traktor <version-number>\collection.nml` |
  | Mac     | `/Users/<your-user-name>/Documents/Native Instruments/Traktor <version-number>/collection.nml`   |

- When the backup is complete, copy the created `collection.nml` file to the above location, overwriting the existing file.
- Open Traktor. Care must be taken to ensure that Traktor doesn't overwrite grid data converted from Rekordbox, otherwise any cue points or loops may not align with the grid. Before loading tracks converted from Rekordbox, it is recommended to either:
  - Enable analysis lock for the tracks, or
  - Right-click the tracks, select `Analyze (Async)`, and tick `Special`, `Key` and `Gain`, and untick `BPM`.
- Now load the converted tracks and check the converted data. Report any problems as GitHub issues in this project.

## Conversion Rules

### From Traktor to Rekordbox

- For each cue point in Traktor:
  - An indexed hot cue is created, with the index matching the index in Traktor.
  - An additional non-indexed memory cue is added for convenience, tagged with the prefix `[djdc]`. These tagged cues will be removed when converting from Rekordbox back to Traktor.
  - If the cue point is a grid cue, a tempo is created.
- The cue point names are copied over as-is.
- Tracks without a playtime are not copied (Rekordbox requires this as total time).
- Tracks without a location set are not copied.
- The cue point types are mapped as follows:

  | Traktor Type | Traktor Colour | Rekordbox Type | Rekordbox Colour   |
  | ------------ | -------------- | -------------- | ------------------ |
  | Cue          | Blue           | Cue            | Green (default)    |
  | Fade-in      | Orange         | Cue            | Pink               |
  | Fade-out     | Orange         | Cue            | Pink               |
  | Load         | Yellow         | Cue            | Yellow (unchanged) |
  | Grid         | White          | Cue            | White (unchanged)  |
  | Loop         | Green          | Loop           | Orange (default)   |

### From Rekordbox to Traktor

- For each indexed hot cue in Rekordbox, a cue point is created, with the index matching the index in Rekordbox.
- For each non-indexed memory cue in Rekordbox, a non-indexed cue point is created, but only if there is no matching indexed hot cue by type and position. This is to avoid unnecessary additional cue point "noise" in Traktor.
- For each tempo in Rekordbox, a non-indexed grid cue point is created, tagged with the prefix `[djdc]`. These tagged cues will be removed and re-created as tempos, when converting from Traktor back to Rekordbox.
- Note that unlike Rekordbox, Traktor grids only allow a single BPM value for the whole track, so if there are multiple tempos with different BPM values in Rekordbox, the result in Traktor will have the same BPM for all grid cue points.
- The hot cue and memory cue names are copied over as-is.
- The hot cue and memory cue types are mapped as follows:

  | Rekordbox Type | Traktor Type |
  | -------------- | ------------ |
  | Cue            | Cue          |
  | Loop           | Loop         |

## Field Mapping

| Field        | Traktor      | Rekordbox    | Copied? |
| ------------ | ------------ | ------------ | ------- |
| Album Title  | Title        | Album        | Yes     |
| Artist       | Artist       | Artist       | Yes     |
| Bpm          | Bpm          | AverageBpm   | Yes     |
| Comments     | Comment      | Comments     | Yes     |
| Date Added   | Import Date  | Date Added   | Yes     |
| Genre        | Genre        | Genre        | Yes     |
| Label        | Label        | Label        | Yes     |
| Total Time   | Playtime     | Total Time   | Yes     |
| Play Count   | Playcount    | Play Count   | No      |
| Track Number | Track        | Track Number | Yes     |
| Track Title  | Title        | Name         | Yes     |
| Year         | Release Date | Year         | No      |

> Some fields are not copied yet. I'm planning to copy them in the future.

## Current Limitations

- The conversion from Rekordbox to Traktor is currently the simplest possible implementation, it cannot merge with an existing Traktor collection.
- If dynamic analysis has been used for a track in Rekordbox, the result in Traktor will be undesirable, with a large number of non-indexed grid point cues sharing the same BPM; Traktor only allows a single BPM value for the whole track.
- For reasons unknown, on some tracks Rekordbox likes to create a large number of tempos, even when the BPM is fixed for the whole track and dynamic analysis has **not** been used. When converting to Traktor, this will currently manifest as (the same) large number of non-indexed grid point cues.
- Disabling the "Store Beatmarker as Hotcue" Traktor setting is not supported.
- Performance is not yet optimal, however a ~10,000 track Traktor or Rekordbox collection should convert in under two minutes.

## Developers

Developers will need to install Java, NodeJS and the Clojure [command line tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).

### Running the Tests

```
clj -Mdev:test-cljs
```

### Starting a CIDER-compatible NREPL Server

```
clj -Mdev:nrepl-server
```

### Compiling the App

```
clj -M:compile-cljs
```

### Packaging the App

```
npm run build-mac
```

## Credits

- [Phoebox](https://github.com/pstare/phoebox) for the suggested cue point colour mapping
- [Digital DJ Tools](https://github.com/digital-dj-tools) for the original version

## License

MIT
