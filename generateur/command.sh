#!/bin/bash

# Vérifie si l'exécutable existe et le supprime s'il est présent.
if [ -f "run" ]; then
    echo "Suppression de l'ancien exécutable run..."
    rm "run"
fi

# Compile le programme COBOL.
cobc -x -o run ./genprog.cbl ./sous-programme/identdiv.cbl ./sous-programme/envirdiv.cbl ./sous-programme/datadiv.cbl ./sous-programme/procediv.cbl

# Exécute le programme COBOL compilé.
./run
