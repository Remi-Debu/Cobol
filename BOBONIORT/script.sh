#!/bin/bash

# Vérifie que les arguments sont spécifiés correctement.
if [ $# -ne 2 ]; then
    echo "Usage: $0 <executable_name> <programme_name>"
    exit 1
fi

# Définis le nom du programme et de l'exécutable à partir des arguments passés en ligne de commande.
PROGRAM=$1
EXECUTABLE=$2

# Vérifie si l'exécutable existe et le supprime s'il est présent.
if [ -f "$EXECUTABLE" ]; then
    echo "Suppression de l'ancien exécutable $EXECUTABLE..."
    rm "$EXECUTABLE"
fi

# Définis les variables d'environnement.
export COB_LDFLAGS="-Wl,--no-as-needed"
export COBCPY="./Copybook"

# Pré-compile avec ocesql
ocesql $PROGRAM.cbl $PROGRAM.cob 

# Compile le programme COBOL.
cobc -locesql -x -o $EXECUTABLE $PROGRAM.cob ./subfolder/subprogram.cob

# Exécute le programme COBOL compilé.
./$EXECUTABLE