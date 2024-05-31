#!/bin/bash

# Vérifie que l'argument est spécifié correctement.
if [ $# -ne 1 ]; then
    echo "Usage: $0 <programme_name>"
    exit 1
fi

# Définis le nom du programme et de l'exécutable à partir des arguments passés en ligne de commande.
PROGRAM=$1

# Vérifie si l'exécutable existe et le supprime s'il est présent.
if [ -f "$PROGRAM.cob" ]; then
    echo "Suppression de l'ancien exécutable $PROGRAM..."
    rm "$PROGRAM.cob"
fi

# Définis les variables d'environnement.
export COB_LDFLAGS="-Wl,--no-as-needed"
export COBCPY="./Copybook"

# Pré-compile avec ocesql
ocesql $PROGRAM.cbl $PROGRAM.cob
