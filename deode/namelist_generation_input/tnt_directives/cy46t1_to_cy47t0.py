#!/usr/bin/env python
# -*- coding: utf-8 -*

# 1. Blocks to be added.
new_blocks = set(['NAMTRAJ','NAMNPROF','NAMSATSIM',
                  ])

# 3. Keys to be moved. If target exists or target block is missing, raise an error.
# Blocks need to be consistent with above blocks movings.
keys_to_move = {('NAMVAR', 'LTRAJGP'): ('NAMTRAJ', 'LTRAJGP'),  # change the key from block, and/or rename it
                ('NAMVAR', 'LREADGPTRAJ'): ('NAMTRAJ', 'LREADGPTRAJ'),
                }

