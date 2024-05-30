#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
## new_blocks = set(['NAMXXX','NAMYYY'])
new_blocks = set(['NAERCLI','NAMDIM_TRAJ','NAMGWDIAG','NAMRIP0','NAMTRANS0','NAMVDF'])

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
keys_to_move = {('NAMARG', 'TSTEP'):('NAMARG', 'UTSTEP'),
               ('NAMCT0', 'LSCMEC'):('NAMPAR0', 'LSCMEC'),
               ('NAMCT0', 'NPRINTLEV'):('NAMPAR0', 'NPRINTLEV'),
               ('NAMCT0', 'LOPT_SCALAR'):('NAMPAR0', 'LOPT_SCALAR'),
               ('NAMCT0', 'LOPT_RS6K'):('NAMPAR0', 'LOPT_RS6K'),
               ('NAMCT0', 'LRFRIC'):('NAMDYNA', 'LRFRIC'),
               ('NAMCT0', 'LRUBC'):('NAMDYNA', 'LRUBC'),
               ('NAMCT0', 'LVERCOR'):('NAMDYNA', 'LVERCOR'),
               ('NAMCT0', 'LPC_FULL'):('NAMDYNA', 'LPC_FULL'),
               ('NAMCT0', 'LPC_CHEAP'):('NAMDYNA', 'LPC_CHEAP'),
               ('NAMCT0', 'LPC_NESCT'):('NAMDYNA', 'LNESCT'),
               ('NAMCT0', 'LPC_NESCV'):('NAMDYNA', 'LNESCV'),
               ('NAMCT0', 'LPC_NESC'):('NAMDYNA', 'LNESC'),
               ('NAMCT0', 'LSLADREP'):('NAMVAR', 'LSLADREP'),
               ('NAMCT0', 'LVECADIN'):('NAMVAR', 'LVECADIN'),
               ('NAMCT0', 'CFPATH'):('NAMOPH', 'CFPATH'),
               ('NAMCT0', 'NTASKS'):('NAMCT0', 'NTASKS_CANARI'),
               ('NAMDYN', 'LSETTLST'):('NAMDYNA', 'LSETTLST'),
               ('NAMDYN', 'LSETTLSV'):('NAMDYNA', 'LSETTLSV'),
               ('NAMDYN', 'LSETTLS'):('NAMDYNA', 'LSETTLS'),
               ('NAMDYN', 'LELTRA'):('NAMDYNA', 'LELTRA'),
               ('NAMDYN', 'TSTEP_TRAJ'):('NAMRIP', 'TSTEP_TRAJ'),
               ('NAMDYN', 'NSMAX_TRAJ'):('NAMDIM_TRAJ', 'NSMAX_TRAJ'),
               ('NAMDYN', 'NSMAX_BACKGR00'):('NAMDIM_TRAJ', 'NSMAX_BACKGR00'),
               ('NAMDYN', 'NSMAX_BACKGR01'):('NAMDIM_TRAJ', 'NSMAX_BACKGR01'),
               ('NAMDYN', 'NSMAX_BACKGR02'):('NAMDIM_TRAJ', 'NSMAX_BACKGR02'),
               ('NAMDYN', 'NSMAX_BACKGR03'):('NAMDIM_TRAJ', 'NSMAX_BACKGR03'),
               ('NAMMODERR', 'NPRTBINS'):('NAMCOSJO', 'NPRTBINS'),
               ('NAMRIP', 'NINDAT'):('NAMRIP0', 'NINDAT'),
               ('NAMRIP', 'NSSSSS'):('NAMRIP0', 'NSSSSS'),
               ('NAMRIP', 'LASTRF'):('NAMRIP0', 'LASTRF'),
               ('NAMTRANS', 'NMAX_RESOL'):('NAMTRANS0', 'NMAX_RESOL'),
               ('NAMTRANS', 'NPROMATR'):('NAMTRANS0', 'NPROMATR'),
               ('NAMTRANS', 'NEPROMATR'):('NAMTRANS0', 'NEPROMATR'),
               ('NAMTRANS', 'RDISTR_E'):('NAMTRANS0', 'RDISTR_E'),
               }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAEAER', 'LAEROPT'),
                      ('NAEAER', 'LAERRAD'),
                      ('NAERAD', 'NCSRADF'),
                      ('NAMAFN', 'GFP_TCGRG'),
                      ('NAMAFN', 'TFP_GRG'),
                      ('NAMGFL', 'YGRG_NL'),
                      ('NAMGFL', 'NGRG'),
                      ('NAMGFL', 'YGRGTEND_NL'),
                      ('NAMGFL', 'YCH4S_NL'),
                      ('NAMGFL', 'LTRCMFIX'),
                      ('NAMGFL', 'NOPTMFIX'),
                      ('NAMGFL', 'NPMFIX'),
                      ('NAMMWAVE', 'LDMWAVE_SENS'),
                      ('NAMTRAJP', 'NPCKFT95'),
                      ('NAMTRAJP', 'NEXPBT95'),
                      ('NAMVAR', 'LJCIMPACT'),
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
blocks_to_remove = set(['NAMCOUPLO4'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
