#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
#new_blocks = set(['NAMZZZ',
#                  ])

# 2. Blocks to be moved. If target block exists, raise an error.
#blocks_to_move = {
#                  }

# 3. Keys to be moved. If target exists or target block is missing, raise an error.
# Blocks need to be consistent with above blocks movings.
keys_to_move = {('NAMFPC', 'NFPSLWIDE'):('NAMFPSC2', 'NFPSLWIDE'),  # change the key from block, and/or rename it
                ('NAMGFL', 'LGHGRTTOV'):('NAMSATS', 'LGHGRTTOV'),
                ('NAMPHY', 'LEDKFI'):('NAMPHY', 'LEDMFI'),
                }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAMCOSJO', 'NGCVVAR'),
                      ('NAMCT0', 'NFRISP'),
                      ('NAMCT0', 'NPISPS'),
                      ('NAMCT1', 'N1ISP'),
                      ('NAMFA', 'COPER'),
                      ('NAMFA', 'NUMOD'),
                      ('NAMFA', 'LPROGRID'),
                      ('NAMFA', 'NHCTPI'),
                      ('NAMFPC', 'LRAW_BIPER'),
                      ('NAMLSFORC', 'NT_NUDG'),
                      ('NAMLSFORC', 'NQV_NUDG'),
                      ('NAMLSFORC', 'NU_NUDG'),
                      ('NAMLSFORC', 'NV_NUDG'),
                      ('NAMMKODB', 'LCHSHIP'),
                      ('NAMMKODB', 'LCHDRIBU'),
                      ('NAMMKODB', 'LSHDRCHO'),
                      ('NAMMKODB', 'LSHDRCHOC'),
                      ('NAMMKODB', 'NMKCMVSE'),
                      ('NAMMKODB', 'LSYNOP'),
                      ('NAMMKODB', 'LCD011'),
                      ('NAMMKODB', 'LCD014'),
                      ('NAMMKODB', 'LCD021'),
                      ('NAMMKODB', 'LCD022'),
                      ('NAMMKODB', 'LCD023'),
                      ('NAMMKODB', 'LCD024'),
                      ('NAMMKODB', 'LAIREP'),
                      ('NAMMKODB', 'LCD041'),
                      ('NAMMKODB', 'LCD141'),
                      ('NAMMKODB', 'LCD142'),
                      ('NAMMKODB', 'LCD144'),
                      ('NAMMKODB', 'LCD145'),
                      ('NAMMKODB', 'LCD241'),
                      ('NAMMKODB', 'LSATOB'),
                      ('NAMMKODB', 'LCD088'),
                      ('NAMMKODB', 'LCD089'),
                      ('NAMMKODB', 'LCD090'),
                      ('NAMMKODB', 'LCD188'),
                      ('NAMMKODB', 'LODRIBU'),
                      ('NAMMKODB', 'LCD063'),
                      ('NAMMKODB', 'LCD064'),
                      ('NAMMKODB', 'LCD165'),
                      ('NAMMKODB', 'LCD160'),
                      ('NAMMKODB', 'LTEMP'),
                      ('NAMMKODB', 'LCD035'),
                      ('NAMMKODB', 'LCD036'),
                      ('NAMMKODB', 'LCD037'),
                      ('NAMMKODB', 'LCD135'),
                      ('NAMMKODB', 'LCD137'),
                      ('NAMMKODB', 'LCD039'),
                      ('NAMMKODB', 'LCD040'),
                      ('NAMMKODB', 'LOPILOT'),
                      ('NAMMKODB', 'LCD032'),
                      ('NAMMKODB', 'LCD033'),
                      ('NAMMKODB', 'LCD034'),
                      ('NAMMKODB', 'LCD131'),
                      ('NAMMKODB', 'LCD134'),
                      ('NAMMKODB', 'LSATEM'),
                      ('NAMMKODB', 'LCD086'),
                      ('NAMMKODB', 'LCD186'),
                      ('NAMMKODB', 'LCD185'),
                      ('NAMMKODB', 'LCD184'),
                      ('NAMMKODB', 'LCD206'),
                      ('NAMMKODB', 'LCD200'),
                      ('NAMMKODB', 'LCD201'),
                      ('NAMMKODB', 'LCD202'),
                      ('NAMMKODB', 'LCD210'),
                      ('NAMMKODB', 'LCD211'),
                      ('NAMMKODB', 'LCD212'),
                      ('NAMMKODB', 'LCD215'),
                      ('NAMMKODB', 'LOPAOB'),
                      ('NAMMKODB', 'LCD180'),
                      ('NAMMKODB', 'LSCATT'),
                      ('NAMMKODB', 'LCD008'),
                      ('NAMMKODB', 'LCD122'),
                      ('NAMMKODB', 'LCD139'),
                      ('NAMMKODB', 'LCD210SC'),
                      ('NAMMKODB', 'LCD300'),
                      ('NAMMKODB', 'LCD301'),
                      ('NAMMKODB', 'LCD302'),
                      ('NAMMKODB', 'LALIMB'),
                      ('NAMMKODB', 'LCD250'),
                      ('NAMMKODB', 'LCD251'),
                      ('NAMOBS', 'LGHGDRYPSH'),
                      ('NAMPHMSE', 'CLSURFEX_FNAME'),
                      ('NAMPHMSE', 'CLPGD_FNAME'),
                      ('NAMPHMSE', 'LFMWRIT'),
                      ('NAMPHMSE', 'LFMREAD'),
                      ('NAMPHMSE', 'LFAPGD'),
                      ('NAMPHMSE', 'LCHKCFA'),
                      ('NAMPHMSE', 'LFP2SX1FA'),
                      ('NAMPHY0', 'GCOMOD'),
                      ('NAMPHY0', 'LFLSMO'),
                      ('NAMPPC', 'LMOVPH'),
                      ('NAMRES', 'COPY'),
                      ('NAMSATS', 'LRTTOV_INTERPOL'),
                      ('NAMVAR', 'LSLADREP'),
                      ('NAMVAR', 'LVECADIN'),
                      ('NAMVAR', 'RCOEFCO2'),
                      ('NAMVAR', 'RCOEFCH4'),
                      ])

# 5. Keys to be set with a value (new or modified). If block is missing, raise an error.
# Blocks need to be consistent with above movings.
## --- none for this cycle ---
## keys_to_set = {('NAMFPD', 'RLATC'):46.5,
##                ('NAMZZZ', 'DODO(0:3)'):[5,6,7],
##                ('NAMVAR', 'LTRAJGP'):True,
##                ('NAMCT0', 'NPOSTS(50)'):-50,
##                ('NAMZZZ', 'THE_NFPCLI'):3,
##                }

# 6. Blocks to be removed. Already missing blocks are ignored.
#blocks_to_remove = set(['NEMWAVELET',
#                        'NAMXXX'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
