#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
## new_blocks = set(['NAMXXX','NAMYYY'])
new_blocks = set(['NAMDIMO','NAMFPMOVE','NAMICE','NAMIO_SERV','NAMOPTCMEM','NAMRADCMEM','NAMVARBC_GBRAD','NAMVARBC_SFCOBS','NAMVV0','NAMWAVELETJB'])

# 2. Blocks to be moved. If target block exists, elements are moved in the existing block.
## blocks_to_move = {'NAMXXX1':'NAMYYY1',
##                  {'NAMXXX2':'NAMYYY2',
##                  }

# 3. Keys to be moved. If target exists or target block is missing, raise an error.
# Blocks need to be consistent with above blocks movings.
# Change the key from block, and/or rename it
## keys_to_move = {('NAMXXX1', 'NVARXXX1'):('NAMYYY1', 'NVARYYY1'),
##                ('NAMXXX2', 'NVARXXX2'):('NAMYYY2', 'NVARYYY2'),
##                }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAEAER', 'RLONVOL'),
                      ('NAEAER', 'RLATVOL'),
                      ('NAERAD', 'LRADONDEM'),
                      ('NAMDIM', 'NUSE_ECCI'),
                      ('NAMDPHY', 'NVEXTRRAD'),
                      ('NAMJO', 'LOW_MODIS_AMV_ERR'),
                      ('NAMMODERR', 'LBGMODERR'),
                      ('NAMMODERR', 'LQEQB'),
                      ('NAMMODERR', 'LJQBAL'),
                      ('NAMMODERR', 'NPERIOD_4DVAR'),
                      ('NAMMODERR', 'L_WARM_START_MODERR'),
                      ('NAMMWAVE', 'RLIMIT_TS'),
                      ('NAMMWAVE', 'RLIMIT_TP'),
                      ('NAMVAR', 'LCO2'),
                      ('NAMVAR', 'LCO2ANER'),
                      ('NAMVRTL', 'LINCV'),
                      ('NAMVRTL', 'LINCVDBG'),
                      ('NAMVRTL', 'RINCVLIM'),
                      ('NAMVRTL', 'RINCVSLOP'),
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
blocks_to_remove = set(['NAMSSMI'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
