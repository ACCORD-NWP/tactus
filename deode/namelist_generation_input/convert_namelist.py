import yaml
import os
import sys
import copy
if len(sys.argv) < 2:
    print("Usage - directive file yaml and yaml and yaml file")
    exit(1)

tnt_directive_filename =sys.argv[1]
namelist_filename =sys.argv[2]


with open(tnt_directive_filename, mode="rt", encoding="utf-8") as file:
    tnt_directives = yaml.safe_load(file)
    file.close()

with open(namelist_filename, mode="rt", encoding="utf-8") as file:
    namelist = yaml.safe_load(file)
    file.close()


new_namelist = copy.deepcopy(namelist)


if "keys_to_move" in tnt_directives:
    for old_block in tnt_directives["keys_to_move"]:
        for old_key in tnt_directives["keys_to_move"][old_block]:
            for new_block in tnt_directives["keys_to_move"][old_block][old_key]:            
                    new_key=tnt_directives["keys_to_move"][old_block][old_key][new_block]                    
                    
                    for namelists_section in namelist:
                        for namelist_block in namelist[namelists_section]:                            
                            if old_block in namelist_block:                                
                                if old_key in namelist[namelists_section][namelist_block]:                                                                        
                                    if not new_block in new_namelist[namelists_section]:
                                        print("Create block ", new_block)
                                        new_namelist[namelists_section][new_block] = dict()                                    
                                    print("Add Key: ", new_block,"\\", new_key, ":", namelist[namelists_section][old_block][old_key])
                                    new_namelist[namelists_section][new_block][new_key]=namelist[namelists_section][old_block][old_key]
                                    print("Delete Key: ", old_block,"\\", old_key)
                                    del new_namelist[namelists_section][old_block][old_key]
        
if "new_blocks" in tnt_directives:
    for new_block in tnt_directives["new_blocks"]:    
        
        for namelists_section in namelist:
            if "f4" in namelists_section:
                for namelist_block in namelist[namelists_section]:                 
                    if new_block in namelist_block:                                
                        print("ERROR: Block existing")
                    else:
                        if (not new_block in new_namelist[namelists_section]):
                            print("Create block ", new_block, " for ", namelists_section)
                            new_namelist[namelists_section][new_block] = dict()                                    
                        
if "blocks_to_move" in tnt_directives:
    for blocks in tnt_directives["blocks_to_move"]:    
        print("Move block", blocks)
        print("ERROR: Not implemented")

if "keys_to_remove" in tnt_directives:
    for blocks in tnt_directives["keys_to_remove"]:    
        print("Delete key", blocks)
                    
        for namelists_section in namelist:
            for namelist_block in namelist[namelists_section]:                            
                if blocks in namelist_block:                                
                    print("Key found")
                    del new_namelist[namelists_section][blocks]


with open(namelist_filename + ".tnt", mode = 'w', encoding="utf-8") as outfile:
    yaml.dump(new_namelist, outfile,  encoding="utf-8",  default_flow_style=False)

exit(0)
