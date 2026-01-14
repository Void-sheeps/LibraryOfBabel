# █████████████████████████████████████████████████████████████████████████████████████████████████████
# esterno- C++
# █████████████████████████████████████████████████████████████████████████████████████████████████████

SAMPLES = {
    "hello": {
        "file": "static/hello.js",
        "language": "javascript",
        "description": "A simple hello world in javascript"
    },
    "hello_hs": {
        "file": "static/hello.hs",
        "language": "haskell",
        "description": "A simple hello world in haskell"
    },
    "library_of_babel": {
        "file": "static/library_of_babel.py",
        "language": "python",
        "description": "A script that generates a random page from the Library of Babel"
    },
    "cat": {
        "file": "static/cat.txt",
        "language": "cat",
        "description": "A simple cat file"
    },
    "thrall": {
        "file": "static/EmpireSilicium/PhylumAlgorithmi/Thrall.hs",
        "language": "haskell",
        "description": "Taxonomic definition of consciousness under Axiom 2026."
    },
    "post_mortem": {
        "file": "static/PostMortem.hs",
        "language": "haskell",
        "description": "Post-mortem analysis of a simulated system."
    },
    "axioma_6502": {
        "file": "static/Axioma6502.asm",
        "language": "asm",
        "description": "Axiom for 6502 processors."
    },
    "suno_extraction": {
        "file": "static/SunoExtraction.hs",
        "language": "haskell",
        "description": "Suno data extraction."
    },
    "swatch_temporal": {
        "file": "static/SwatchTemporal.hs",
        "language": "haskell",
        "description": "A script that processes temporal data."
    }
}

# █████████████████████████████████████████████████████████████████████████████████████████████████████
# concettuale - Haskell
# █████████████████████████████████████████████████████████████████████████████████████████████████████

import subprocess
import sys
import os
import re
import tempfile
import pty

def get_conceptual_script(script_name):
    return SAMPLES.get(script_name)

def run_script(script_path, language, action, args, script_name):
    if action == "route":
        if language == "javascript":
            subprocess.run(["node", script_path] + args)
        elif language == "haskell":
            with tempfile.NamedTemporaryFile(mode='w+', delete=False, suffix='.out', dir='/tmp') as tempf:
                executable_path = tempf.name

            # Extrair o diretório base do script para o include path
            script_dir = os.path.dirname(script_path)
            compile_command = ["ghc", "-o", executable_path, script_path]
            if script_dir:
                compile_command.insert(1, f"-i{script_dir}")

            compile_result = subprocess.run(compile_command, capture_output=True, text=True)

            if compile_result.returncode != 0:
                print(f"Erro de compilação Haskell: {compile_result.stderr}")
                return

            subprocess.run([executable_path] + args)
            os.remove(executable_path)

            # Remover arquivos .o e .hi se existirem
            base_name = os.path.splitext(script_path)[0]
            if os.path.exists(base_name + ".o"):
                os.remove(base_name + ".o")
            if os.path.exists(base_name + ".hi"):
                os.remove(base_name + ".hi")

        elif language == "haskell_tui":
            with tempfile.NamedTemporaryFile(mode='w+', delete=False, suffix='.out', dir='/tmp') as tempf:
                executable_path = tempf.name

            compile_result = subprocess.run(["ghc", "-o", executable_path, script_path], capture_output=True, text=True)

            if compile_result.returncode != 0:
                print(f"Erro de compilação Haskell: {compile_result.stderr}")
                return

            pty.spawn([executable_path] + args)
            os.remove(executable_path)

            base_name = os.path.splitext(script_path)[0]
            if os.path.exists(base_name + ".o"):
                os.remove(base_name + ".o")
            if os.path.exists(base_name + ".hi"):
                os.remove(base_name + ".hi")

        elif language == "python":
            subprocess.run(["python3", script_path] + args)
        elif language == "cat":
            with open(script_path, 'r') as f:
                print(f.read())
        elif language == "asm":
            print("Assembly file, content:\n")
            with open(script_path, 'r') as f:
                print(f.read())
    elif action == "path":
        print(script_path)

def main():
    if len(sys.argv) < 2:
        print("Uso: python conceptual_proxy.py <nome_do_script> [--action <route|path>] [args...]")
        return

    script_name = sys.argv[1]

    action = "route"
    try:
        action_index = sys.argv.index("--action")
        if action_index + 1 < len(sys.argv):
            action = sys.argv[action_index + 1]
            sys.argv.pop(action_index)
            sys.argv.pop(action_index)
    except ValueError:
        pass

    args = sys.argv[2:]

    script_info = get_conceptual_script(script_name)

    if script_info:
        run_script(script_info["file"], script_info["language"], action, args, script_name)
    else:
        print(f"Script '{script_name}' não encontrado.")

if __name__ == "__main__":
    main()
