# runsushi

## Requirement

* [SusHi](https://sushi.hepforge.org/) linking with [2HDMC](https://2hdmc.hepforge.org/) 1.7.0: make sure that it has been built with `make predef=2HDMC`.
* [h2decays](https://github.com/cbpark/h2decays): [stack](https://docs.haskellstack.org/en/stable/README/) users can install it without building it explicitly. See [`stack.yaml`](./stack.yaml).

See the patches for `SusHi` and `2HDMC`:

* [`sushi_gsl_path_fix.patch`](https://gist.github.com/cbpark/ada5742bd68891e1481dd8d8e1aa9861),
* [`2HDMC_170.patch`](https://gist.github.com/cbpark/fc05adadbd52c3fb0319be28dcdc1c1b).

Furthermore, `NNPDF31` PDF sets must be installed to run `sushi`.

```
for pdf in NNPDF31_lo_as_0118 NNPDF31_nlo_as_0118 NNPDF31_nnlo_as_0118; \
    do sudo lhapdf install --upgrade "$pdf"; \
done
```

## Usage

See the help messages:

```
$ runsushi --help
Run SuSHi to obtain the cross sections

Usage: runsushi --sushi STRING [--input STRING] [--eCM DOUBLE] [--mtype INT]
                [--mH DOUBLE...] [--mA DOUBLE] --mHp DOUBLE [--mS DOUBLE]
                --tanb DOUBLE --cosba DOUBLE [--stepsize DOUBLE]
                [--output STRING]

Available options:
  -h,--help                Show this help text
  --sushi STRING           SuSHi executable (which sushi)
  --input STRING           template for the input to SuSHi (default
                           input_template.in)
  --eCM DOUBLE             center-of-mass energy in GeV (default: 13000 GeV)
  --mtype INT              model type (either 1 or 2)
  --mH DOUBLE...           heavy Higgs mass
  --mA DOUBLE              CP-odd Higgs mass
  --mHp DOUBLE             charged Higgs mass
  --mS DOUBLE              heavy mass scale (m_A if MSSM)
  --tanb DOUBLE            tan(beta)
  --cosba DOUBLE           cos(beta-alpha)
  --stepsize DOUBLE        step size (default: 0.5)
  --output STRING          the name of the output file

$ runsushiHpW --help
Run SusHi and h2decays to obtain the cross sections

Usage: runsushiHpW --h2decays STRING --sushi STRING [--input STRING]
                   [--eCM DOUBLE] [--mtype INT] [--mH DOUBLE...] [--mA DOUBLE]
                   --mHp DOUBLE [--mS DOUBLE] --tanb DOUBLE --cosba DOUBLE
                   [--stepsize DOUBLE] [--output STRING]

Available options:
  -h,--help                Show this help text
  --h2decays STRING        the executable path of h2decays
  --sushi STRING           the executable path of SusHi
  --input STRING           template for the input to SuSHi (default
                           input_template.in)
  --eCM DOUBLE             center-of-mass energy in GeV (default: 13000 GeV)
  --mtype INT              model type (either 1 or 2)
  --mH DOUBLE...           heavy Higgs mass
  --mA DOUBLE              CP-odd Higgs mass
  --mHp DOUBLE             charged Higgs mass
  --mS DOUBLE              heavy mass scale (m_A if MSSM)
  --tanb DOUBLE            tan(beta)
  --cosba DOUBLE           cos(beta-alpha)
  --stepsize DOUBLE        step size (default: 0.5)
  --output STRING          the name of the output file
```
