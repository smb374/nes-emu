# nes-emu

A NES emu in Rust

## Features

- Implements full CPU instruction emulation.
- Passing 100 tests out of 136 tests from [100thCoin/AccuracyCoin](https://github.com/100thCoin/AccuracyCoin)
- Support iNES mapper 0-4, 7
- Accurate PPU VBLANK timing & NMI control
- Plays Battletoads
- Battery save support through whole RAM snapshot

## Building

Make sure you have SDL2 installed for the main branch (egui is used in separated branch)
for static linking, other than that just run:

```sh
cargo build --release
```

To build the emulator then use

```sh
./target/release/emulator <rom path>
```

To start the emulator and load the ROM.

## Bindings

Currently only supports single controller with static binding:

| Key        | Controller |
|------------|------------|
| Z          | A          |
| X          | B          |
| Return     | Start      |
| Space      | Select     |
| Arrow keys | D-Pad      |

## Test Results

AccuracyCoin run all result screen:
![AccuracyCoin Result](./screenshots/AccuracyCoinResult.png)

## Gameplay Screenshots

![Battletoads Map](./screenshots/battletoads_stage_map.png)
![Battletoads Turbo Tunnel](./screenshots/battletoads_turbo_tunnel.png)
![Crystalis](./screenshots/crystalis.png)
![Ninja Gaiden](./screenshots/ninja_gaiden.png)
![Zelda](./screenshots/zelda.png)

## Roadmap

- [ ] ~~Improve DMA accuracy by implementing RDY line control, get/put cycles, and
  cycle alignment rather than current hard-coded stuff~~
  Implementation done, but not passing tests.
- [ ] Implement correct instruction behavior for SHA, SHS, SHX, SHY to
  account for RDY line assertion when DMA occurrence
  - Unknown requirements for this.
- [ ] Improve NMI overlapping behavior
- [ ] Improve PPU accuracy
- [ ] Improve APU accuracy
- [ ] Add randomized RAM initialization for components
- [ ] Add widgets for emulator UI
- [ ] Add save state support
