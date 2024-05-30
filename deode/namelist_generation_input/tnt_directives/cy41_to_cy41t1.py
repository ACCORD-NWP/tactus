#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
## new_blocks = set(['NAMXXX','NAMYYY'])
new_blocks = set(['NAMFAINIT'])

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
keys_to_move = {('NEMELBC0A', 'RTENC'):('NEMELBC0B', 'RTENC'),
               ('NEMELBC0A', 'NEK0'):('NEMELBC0B', 'NEK0'),
               ('NEMELBC0A', 'NEK1'):('NEMELBC0B', 'NEK1'),
               ('NEMELBC0A', 'NEFRSPCPL'):('NEMELBC0B', 'NEFRSPCPL'),
               ('NEMELBC0A', 'NEN1'):('NEMELBC0B', 'NEN1'),
               ('NEMELBC0A', 'NEN2'):('NEMELBC0B', 'NEN2'),
               ('NEMELBC0A', 'SPNUDVOR'):('NEMELBC0B', 'SPNUDVOR'),
               ('NEMELBC0A', 'SPNUDDIV'):('NEMELBC0B', 'SPNUDDIV'),
               ('NEMELBC0A', 'SPNUDT'):('NEMELBC0B', 'SPNUDT'),
               ('NEMELBC0A', 'SPNUDQ'):('NEMELBC0B', 'SPNUDQ'),
               ('NEMELBC0A', 'SPNUDSP'):('NEMELBC0B', 'SPNUDSP'),
               ('NEMELBC0A', 'RNUTENC'):('NEMELBC0B', 'RNUTENC'),
               ('NEMELBC0B', 'ZEPA_GMV'):('NEMELBC0B', 'EPA_GMV'),
               ('NEMELBC0B', 'ZEPA_GMVS'):('NEMELBC0B', 'EPA_GMVS'),
               ('NEMELBC0B', 'ZEPA_GFL'):('NEMELBC0B', 'EPA_GFL'),
               }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAMAFN', 'TFP_SCC2'),
                      ('NAMAFN', 'TFP_GCCA'),
                      ('NAMFPC', 'LAGGED_BIPER'),
                      ('NAMFPF', 'LNEWFPUF'),
                      ('NAMFPF', 'NEXPMFPSF'),
                      ('NAMGFL', 'YSCC2_NL'),
                      ('NAMGFL', 'YGCCA_NL'),
                      ('NAMMTS', 'NSAT'),
                      ('NAMMTS', 'NHLIM'),
                      ('NAMPHY0', 'ETKE_RIFC'),
                      ('NAMPHY0', 'ETKE_KA2A1'),
                      ('NAMPHY0', 'ETKE_KA2A3'),
                      ('NAMPHY0', 'LGMT'),
                      ('NAMPHY0', 'GMUSE'),
                      ('NAMPHY0', 'GMURW'),
                      ('NAMPHY0', 'GALSIN'),
                      ('NAMPHY0', 'LGPSMI'),
                      ('NAMPHY0', 'LGVARDET'),
                      ('NAMPHY3', 'FCM_A'),
                      ('NAMPHY', 'LCOEFK_QNSE'),
                      ('NAMPHY', 'LCOEFK_CCH02A'),
                      ('NAMPHY', 'LCOEFK_RIM'),
                      ('NAMPHY', 'LCOEFK_SCC'),
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

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
