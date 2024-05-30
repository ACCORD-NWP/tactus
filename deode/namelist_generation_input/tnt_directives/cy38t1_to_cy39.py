#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
## new_blocks = set(['NAMXXX','NAMYYY'])
new_blocks = set(['NAMCVER','NAEVOL','NAMCHEM'])

# 2. Blocks to be moved. If target block exists, elements are moved in the existing block.
## blocks_to_move = {'NAMXXX1':'NAMYYY1',
##                  {'NAMXXX2':'NAMYYY2',
##                  }
blocks_to_move = {'NAMJO':'NAMCOSJO',
                 {'NAMTLEVOL':'NAEPHLI',
                 {'NAMVFP':'NAMVAR',
                 }

# 3. Keys to be moved. If target exists or target block is missing, raise an error.
# Blocks need to be consistent with above blocks movings.
# Change the key from block, and/or rename it
## keys_to_move = {('NAMXXX1', 'NVARXXX1'):('NAMYYY1', 'NVARYYY1'),
##                ('NAMXXX2', 'NVARXXX2'):('NAMYYY2', 'NVARYYY2'),
##                }
keys_to_move = {('NAMCT0', 'LAPRXPK'):('NAMDYNA', 'LAPRXPK'),
                ('NAMCT0', 'LVERTFE'):('NAMCVER', 'LVERTFE'),
                ('NAMCT0', 'NVSCH'):('NAMCVER', 'NVSCH'),
                ('NAMDYN', 'NDLNPR'):('NAMDYNA', 'NDLNPR'),
                ('NAMDYN', 'RC_PD1'):('NAMDYNA', 'RC_PD1'),
                ('NAMDYN', 'NITPRHS'):('NAMDYNA', 'NITPRHS'),
                ('NAMDYN', 'LRNHC1'):('NAMCVER', 'LRNHC1'),
                ('NAMDYN', 'LVFE_LAPL'):('NAMCVER', 'LVFE_LAPL'),
                ('NAMDYN', 'LVFE_LAPL_BC'):('NAMCVER', 'LVFE_LAPL_BC'),
                ('NAMDYN', 'LVFE_X_TERM'):('NAMCVER', 'LVFE_X_TERM'),
                ('NAMDYN', 'LVFE_GW'):('NAMCVER', 'LVFE_GW'),
                ('NAMDYN', 'LVFE_Z_TERM'):('NAMCVER', 'LVFE_Z_TERM'),
                ('NAMDYN', 'LVFE_GWMPA'):('NAMCVER', 'LVFE_GWMPA'),
                ('NAMDYN', 'LVFE_DELNHPRE'):('NAMCVER', 'LVFE_DELNHPRE'),
                ('NAMDYN', 'LVFE_DBCS'):('NAMCVER', 'LVFE_DBCS'),
                ('NAMDYN', 'LVFE_DBCT'):('NAMCVER', 'LVFE_DBCT'),
                ('NAMDYN', 'LVFE_DERIB'):('NAMCVER', 'LVFE_DERIB'),
                ('NAMFPC', 'LFITP'):('NAMFPC', 'NFITP'),
                ('NAMFPC', 'LFITT'):('NAMFPC', 'NFITT'),
                ('NAMFPC', 'LFITV'):('NAMFPC', 'NFITV'),
                ('NAMFPC', 'LFITI'):('NAMFPC', 'NFITI'),
                ('NAMFPG', 'LFPDISTRIB'):('NAMFPG', 'NFPDISTRIB'),
                ('NAMINI', 'NEINI'):('NAMINI', 'LDFI'),
               }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NEMELBC0A', 'NBICOQ'),
                      ('NEMFPEZO', 'NFPROMEL'),
                      ('NAEAER', 'NAERDUT'),
                      ('NAEAER', 'NBCOPT'),
                      ('NAEAER', 'NDDOPT'),
                      ('NAEAER', 'NOMOPT'),
                      ('NAEAER', 'NSSOPT'),
                      ('NAEAER', 'NSUOPT'),
                      ('NAMCUMFS', 'LECUMFS2'),
                      ('NAMDFI', 'LDIVONLY'),
                      ('NAMDIM', 'NXMAX'),
                      ('NAMDIM', 'NPROMB'),
                      ('NAMDIM', 'NPROMC'),
                      ('NAMDYN', 'LLN2HYD'),
                      ('NAMFPC', 'LFITF'),
                      ('NAMFPC', 'NFPINCR'),
                      ('NAMFPD', 'NFPBMSMAX'),
                      ('NAMFPD', 'NFPBSMAX'),
                      ('NAMGFL', 'LGRGSFC'),
                      ('NAMGFL', 'LSF6SFC'),
                      ('NAMONEDVAR', 'LERAIN_CLEARSKY'),
                      ('NAMONEDVAR', 'RLIMIT_LS'),
                      ('NAMPAR1', 'LOMPDIST'),
                      ('NAMPAR1', 'LCPG_SPLIT'),
                      ('NAMPHY', 'L2PHYS'),
                      ('NAMTRAJP', 'LECUBM2'),
                      ('NAMVAR', 'LJCNMTL'),
                      ('NAMVAR', 'LUSEJCNMI'),
                      ('NAMVAR', 'LAEOLUSL2BP'),
                      ('NAMVAREPS', 'L_INC_HDIF'),
                      ('NAMVAREPS', 'NRES_MA'),
                      ('NAMVAREPS', 'NRES_MB'),
                      ('NAMVAREPS', 'NFCHO_HDIF_TRANS'),
                      ('NAMVAREPS', 'HDIRVOR_MA'),
                      ('NAMVAREPS', 'HDIRDIV_MA'),
                      ('NAMVAREPS', 'HDIRT_MA'),
                      ('NAMVAREPS', 'HDIRQ_MA'),
                      ('NAMVAREPS', 'HDIRVOR_MB'),
                      ('NAMVAREPS', 'HDIRDIV_MB'),
                      ('NAMVAREPS', 'HDIRT_MB'),
                      ('NAMVAREPS', 'HDIRQ_MB'),
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
blocks_to_remove = set(['NAMGOM','NAMNMI','NAMSKF'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
