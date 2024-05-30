#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 1. Blocks to be added.
new_blocks = set(['NAETLDIAG','NAMAERDET','NAMCOMPO','NAMMETHOX','NAMSPP'])

# 2. Blocks to be moved. If target block exists, raise an error.
## --- none for this cycle ---
## blocks_to_move = {
##                  }

# 3. Keys to be moved. If target exists or target block is missing, raise an error.
# Blocks need to be consistent with above blocks movings.
# Change the key from block, and/or rename it
## --- none for this cycle ---
## keys_to_move = {('NAMXXX1', 'NVARXXX1'):('NAMYYY1', 'NVARYYY1'),
##                ('NAMXXX2', 'NVARXXX2'):('NAMYYY2', 'NVARYYY2'),
##                }

# 4. Keys to be removed. Already missing keys are ignored.
# Blocks need to be consistent with above movings.
keys_to_remove = set([('NAMCHEM', 'LCHEM_DIA'),
                      ('NAMCHEM', 'LCHEM_DDFLX'),
                      ('NAMCHEM', 'LCHEM_TROPO'),
                      ('NAMCLDDET', 'N__NUM_AEROSOL_TESTS'),
                      ('NAMCLDDET', 'L__DO_AEROSOLDETECTION'),
                      ('NAMCLDDET', 'N__NUM_AEROSOL_CHANS'),
                      ('NAMCLDDET', 'N__AEROSOL_CHANS'),
                      ('NAMCLDDET', 'R__AEROSOL_ABSCISSAE'),
                      ('NAMCLDDET', 'R__AEROSOL_THRESHOLD'),
                      ('NAMCLDDET', 'R__AEROSOLMINNORM'),
                      ('NAMCT0', 'LGRBOP'),
                      ('NAMCT0', 'LRETCFOU'),
                      ('NAMCT0', 'LWRTCFOU'),
                      ('NAMCT0', 'LRFOUTCNORM'),
                      ('NAMCT0', 'LRGPTCNORM'),
                      ('NAMCVER', 'LRNHC1'),
                      ('NAMCVER', 'LVFE_DBCS'),
                      ('NAMCVER', 'LVFE_DBCT'),
                      ('NAMDIM', 'NTCMAX'),
                      ('NAMDYNA', 'LGWOPT1'),
                      ('NAMDYNCORE', 'LDYNCORE'),
                      ('NAMDYNCORE', 'LVERTINI'),
                      ('NAMGFL', 'LAEROSFC'),
                      ('NAMGFL', 'LAERODIU'),
                      ('NAMGFL', 'LCO2SFC'),
                      ('NAMGFL', 'LFIRE'),
                      ('NAMGFL', 'LINJ'),
                      ('NAMGFL', 'LINJ_CHEM'),
                      ('NAMLCZ', 'LJACDAV'),
                      ('NAMLCZ', 'L_UNPREDICTABLE_COMPONENTS'),
                      ('NAMSTOPH', 'LSTOPH'),
                      ('NAMSTOPH', 'NFRSTOPH'),
                      ('NAMSTOPH', 'AMAGSTOPH'),
                      ('NAMSTOPH', 'ADLATSTOPH'),
                      ('NAMSTOPH', 'ADLONSTOPH'),
                      ('NAMTRANS', 'LFFT992'),
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
blocks_to_remove = set(['NAMONEDVAR','NAMPONG'])

# 7. Macros: substitutions in the namelist's values. A *None* value ignores
# the substitution (keeps the keyword, to be substituted later on.
macros = {'PERTURB':None,
          }
