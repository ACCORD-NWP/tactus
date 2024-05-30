#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
## new_blocks = set(['NAMXXX','NAMYYY'])
new_blocks = set(['NAMFPDYF'])

# 2. Blocks to be moved. If target block exists, raise an error.
## --- none for this cycle ---
## blocks_to_move = {
##                  }

# 3. Keys to be moved. If target exists or target block is missing, raise an error.
# Blocks need to be consistent with above blocks movings.
# Change the key from block, and/or rename it
## keys_to_move = {('NAMFPD', 'RFPBSCAL'):('NAMFPC', 'RFPBSCAL'),
##                ('NAMDYN', 'LDRY_ECMWF'):('NAMDYNA', 'LDRY_ECMWF'),
##                }
## keys_to_move = {('NAMXXX1', 'NVARXXX1'):('NAMYYY1', 'NVARYYY1'),
##                ('NAMXXX2', 'NVARXXX2'):('NAMYYY2', 'NVARYYY2'),
##                }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAMPARAR', 'LMNHLEV'),
                      ('NAMSENS', 'LMINI'),
                      ('NAMVAR', 'LAVARC'),
                      ('NAMVRTL', 'L801TL'),
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
### blocks_to_remove = set(['NAMXXX','NAMYYY'])
blocks_to_remove = set(['NAMHCP'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
