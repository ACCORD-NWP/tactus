#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
## new_blocks = set(['NAMXXX','NAMYYY'])
new_blocks = set(['NAMACV'])

# 2. Blocks to be moved. If target block exists, elements are moved in the existing block.
## --- none for this cycle ---
## blocks_to_move = {'NAMXXX1':'NAMYYY1',
##                  {'NAMXXX2':'NAMYYY2',
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
keys_to_move = {('NAEAER', 'LAERLISI'):('NAMGFL', 'LAERLISI'),
               ('NAEAER', 'LAERNITRATE'):('NAMCOMPO', 'LAERNITRATE'),
               ('NAMARG', 'UTSTEP'):('NAMRIP', 'TSTEP'),
               ('NAMARG', 'CUSTOP'):('NAMRIP', 'CSTOP'),
               ('NAMRIP', 'NSTOP'):('NAMRIP', 'CSTOP'),
               ('NAMCT0', 'LSLPHY'):('NAEPHY', 'LSLPHY'),
               ('NAMCT0', 'LNHDYN'):('NAMCT0', 'LNHEE'),
               ('NAMDYN', 'RPRES_SETTLSTF'):('NAMDYN', 'RPRES_SETTLSVF'),
               }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAEAER', 'LUVINDX'),
                      ('NAEAER', 'LAERCALIP'),
                      ('NAERAD', 'LEO3VAR'),
                      ('NAERAD', 'LGHGCMIP5'),
                      ('NAMARG', 'TSTEP'),
                      ('NAMCOMPO', 'LCHEM_DDFLX_DIR'),
                      ('NAMCOMPO', 'LAERODIU'),
                      ('NAMCOSJO', 'NPRTBINS'),
                      ('NAMCOSJO', 'LQSCATT'),
                      ('NAMCOSJO', 'WSCAT5_FAC'),
                      ('NAMCOSJO', 'WSCAT6_FAC'),
                      ('NAMCT0', 'LNOBGON'),
                      ('NAMDYNA', 'LVERCOR'),
                      ('NAMDYNA', 'LRWSDLW'),
                      ('NAMDYNA', 'LRWSDLR'),
                      ('NAMDYNA', 'LRWSDLR2'),
                      ('NAMDYNA', 'LRWSDLG'),
                      ('NAMDYNA', 'NITPRHS'),
                      ('NAMDYNA', 'LCURVW'),
                      ('NAMDYNA', 'RC_PD1'),
                      ('NAMDYN', 'VCAK'),
                      ('NAMDYN', 'VCTR'),
                      ('NAMDYN', 'VCPR'),
                      ('NAMDYN', 'LADVFW'),
                      ('NAMDYN', 'SLHDKREF'),
                      ('NAMDYN', 'RMAX_SETTLSTF'),
                      ('NAMGFL', 'YTRAC_NL'),
                      ('NAMGFL', 'NTRAC'),
                      ('NAMMWAVE', 'LDREVISED_AZIMUTH'),
                      ('NAMOBS', 'NOBSHOR'),
                      ('NAMPAR1', 'LPPTSF'),
                      ('NAMPAR1', 'NPPBUFLEN'),
                      ('NAMPAR1', 'NPPFILES'),
                      ('NAMSPSDT', 'LSPPTGFIX'),
                      ('NAMVAR', 'LTSCV'),
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
blocks_to_remove = set(['NAMTS'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
