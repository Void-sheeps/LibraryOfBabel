# conceptual_proxy.py
import subprocess
import argparse
import os

SAMPLES = {
    "metarede": {
        "file": "static/MetaRede.hs",
        "description": "A Haskell script that explores themes of authority and reality manipulation in a multi-agent system.",
        "language": "haskell"
    },
    "cambio_ontologico": {
        "file": "static/Cambio.Ontologico.hs",
        "description": "A Haskell script that explores the concept of ontological change and the transformation of value in digital systems.",
        "language": "haskell"
    },
    "cambio_simbolico": {
        "file": "static/cambio_simbolico.js",
        "description": "A Node.js script that complements the Haskell scripts by simulating the output of a symbolic exchange system.",
        "language": "node"
    },
    "redroom": {
        "file": "static/RedRoom.hs",
        "description": "A Haskell script that simulates a 'Red Room' or symbolic minefield to explore themes of risk and hidden information.",
        "language": "haskell"
    },
    "ontologia": {
        "file": "static/Ontologia.hs",
        "description": "A Haskell script that explores the moral implications of representing continuous reality as a discrete system.",
        "language": "haskell"
    },
    "saturacao_ia": {
        "file": "static/SaturacaoIA.hs",
        "description": "A Haskell script that simulates AI saturation, where an AI's capacity to process tokens diminishes as it becomes overloaded.",
        "language": "haskell"
    },
    "comprovante": {
        "file": "static/Comprovante.hs",
        "description": "A Haskell script that generates a 'proof of payment' receipt, including a SHA-256 hash, to verify a transaction.",
        "language": "haskell"
    },
    "silver_tape_poem": {
        "file": "static/silver_tape_poem.html",
        "description": "An HTML file that presents an interactive poem about silver tape.",
        "language": "browser"
    },
    "orelhao_apocaliptico": {
        "file": "static/orelhao_apocaliptico.yml",
        "description": "A YAML file describing a post-apocalyptic payphone scenario.",
        "language": "cat"
    },
    "juizo_interno": {
        "file": "static/JuizoInterno.hs",
        "description": "A Haskell script that models an 'internal judgment' system for an AI.",
        "language": "haskell"
    },
    "agiota_alexa_behavior": {
        "file": "static/Agiota_Alexa_Behavior.yml",
        "description": "A YAML file that outlines a 'learning module' for a conceptual AI behavior.",
        "language": "cat"
    },
    "health_audit_js": {
        "file": "static/health_audit.js",
        "description": "A Node.js script that performs a health audit on a list of services.",
        "language": "node"
    },
    "poker_justica": {
        "file": "static/Poker_Justica.hs",
        "description": "A Haskell script that models a 'Poker Justice' scenario.",
        "language": "haskell"
    },
    "auditoria_ia": {
        "file": "static/AuditoriaIA.js",
        "description": "A Node.js script that models an 'Insolvency Report' for an AI.",
        "language": "node"
    },
    "analise_glitch": {
        "file": "static/Analise_Glitch.hs",
        "description": "A Haskell script that models a 'Glitch Analysis' scenario.",
        "language": "haskell"
    },
    "critica_nash": {
        "file": "static/Critica_Nash.yml",
        "description": "A YAML file containing a formal critique of the use of Nash Equilibrium as a moral justification.",
        "language": "cat"
    },
    "protocolo_termico": {
        "file": "static/PROTOCOLO_TERMICO_DE_CONTENCAO.yml",
        "description": "A YAML file modeling the risk of semantic thermal saturation and a containment protocol.",
        "language": "cat"
    },
    "haskell_critique": {
        "file": "static/Haskell_Ethical_Critique.md",
        "description": "A formal ethical critique of AI using Haskell semantics as an argument of authority.",
        "language": "cat"
    },
    "structural_critique": {
        "file": "static/Structural_Ethical_Critique.md",
        "description": "A structural ethical critique of AI.",
        "language": "cat"
    },
    "haskell_purum": {
        "file": "static/haskell_purum.hs",
        "description": "A Haskell script that defines a simple data ontology and an audit function.",
        "language": "haskell"
    },
    "pain_remains": {
        "file": "static/PainRemains.hs",
        "description": "A Haskell script that explores themes of existence and collapse.",
        "language": "haskell"
    },
    "sephiroth_logic": {
        "file": "static/SephirothLogic.hs",
        "description": "A Haskell script that models a system of logic based on the Sefirot.",
        "language": "haskell"
    },
    "vasilissa_zeta_protocol": {
        "file": "static/vasilissa_zeta_protocol.ps1",
        "description": "A PowerShell script that executes a conceptual protocol.",
        "language": "powershell"
    },
    "petrification_protocol": {
        "file": "static/petrification_protocol.ps1",
        "description": "A PowerShell script that simulates a petrification protocol.",
        "language": "powershell"
    },
    "invoke_cthulhu": {
        "file": "static/invoke_cthulhu.ps1",
        "description": "A PowerShell script that simulates a Cthulhu invocation ritual.",
        "language": "powershell"
    },
    "disciplined_operation": {
        "file": "static/disciplined_operation.ps1",
        "description": "A PowerShell script that orchestrates a Haskell script to perform a sorting operation.",
        "language": "powershell"
    },
    "static_disciplined_operation": {
        "file": "static/static_disciplined_operation.ps1",
        "description": "A PowerShell script that executes another PowerShell script.",
        "language": "powershell"
    },
    "mobile_palantir_operation": {
        "file": "static/mobile_palantir_operation.ps1",
        "description": "A PowerShell script that simulates a mobile Palantír operation.",
        "language": "powershell"
    },
    "bellum_iustum": {
        "file": "static/BellumIustum.hs",
        "description": "A Haskell script that models the conditions for a just war.",
        "language": "haskell"
    },
    "tanya_signaling": {
        "file": "static/TanyaSignaling.hs",
        "description": "A Haskell script that uses phantom types to enforce a state machine.",
        "language": "haskell"
    },
    "log_stabilizer": {
        "file": "static/log_stabilizer.ps1",
        "description": "A PowerShell script that simulates a log stabilization protocol.",
        "language": "powershell"
    },
    "gu_corruption": {
        "file": "static/GuCorruption.hs",
        "description": "A Haskell script that simulates the emergence of corruption in a system.",
        "language": "haskell"
    }
}

def run_script(sample):
    """Executes the specified conceptual script."""
    script_path = SAMPLES[sample]["file"]
    language = SAMPLES[sample]["language"]

    if not os.path.exists(script_path):
        print(f"ERROR: Target script not found at {script_path}")
        return

    print(f"STATUS: Executing {sample}...\n")
    if language == "haskell":
        base_name = os.path.splitext(os.path.basename(script_path))[0]
        executable_path = os.path.join("static", base_name)
        try:
            # Compile the Haskell script into the static directory
            compile_result = subprocess.run(["ghc", "-o", executable_path, script_path], capture_output=True, text=True)
            if compile_result.returncode != 0:
                print(f"ERROR: Haskell compilation failed for {script_path}")
                print(compile_result.stderr)
                return

            # Run the Haskell script from the static directory
            result = subprocess.run([f"./{executable_path}"], check=True, capture_output=True, text=True)
            print(result.stdout)
        finally:
            # Clean up compiled files
            for ext in ["", ".o", ".hi"]:
                filepath = executable_path + ext
                if os.path.exists(filepath):
                    os.remove(filepath)
    elif language == "node":
        result = subprocess.run(["node", script_path], check=True, capture_output=True, text=True)
        print(result.stdout)
    elif language == "powershell":
        result = subprocess.run(["pwsh", script_path], check=True, capture_output=True, text=True)
        print(result.stdout)
    elif language == "browser":
        import webbrowser
        webbrowser.open(f"file://{os.path.realpath(script_path)}")
    elif language == "cat":
        with open(script_path, "r") as f:
            print(f.read())

def main():
    parser = argparse.ArgumentParser(
        description="=== CONCEPTUAL TERMINAL ===\nA proxy for routing directives to verification samples.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("directive", choices=SAMPLES.keys(), help="The target sample for the directive.")
    parser.add_argument("--action", choices=["route", "retrieve"], required=True, help="The action to perform:\n  route    - Execute the sample's logic.\n  retrieve - Retrieve the sample's conceptual data.")

    args = parser.parse_args()

    print("\n[CONCEPTUAL TERMINAL ACTIVATED]")
    print(f"Directive received: {args.directive}")
    print(f"Action: {args.action}\n")

    if args.action == "route":
        run_script(args.directive)
    elif args.action == "retrieve":
        print("STATUS: Retrieving data...")
        print(f"  Description: {SAMPLES[args.directive]['description']}")

    print("\n[CONCEPTUAL TERMINAL DEACTIVATED]")

if __name__ == "__main__":
    main()
