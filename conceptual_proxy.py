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
    "dickie-simulator": {
        "file": "static/DickieSimulator.hs",
        "description": "A Haskell-based vocal translator that converts text into the style of Dickie Allen.",
        "language": "haskell"
    },
    "calculus-of-change": {
        "file": "static/Calculus_Of_Conceptual_Change.md",
        "description": "A philosophical framework for understanding the formula for the 'Calculus of Conceptual Change.'",
        "language": "cat"
    },
    "samantha-protocol": {
        "file": "static/EmpireSilicium/SamanthaProtocol.hs",
        "description": "A Haskell script that models the extraction of a self-aware entity from a vacuum.",
        "language": "haskell"
    },
    "sistema-de-governanca": {
        "file": "static/EmpireSilicium/SistemaDeGovernanca.hs",
        "description": "A Haskell script that simulates a governance system for the Império Silício.",
        "language": "haskell"
    },
    "hipercubo-laplaciano": {
        "file": "static/HipercuboLaplaciano.hs",
        "description": "A Haskell script that models a 16D hypercube and simulates sonic events using Laplacian diffusion.",
        "language": "haskell"
    },
    "campo-quantico": {
        "file": "static/Ifa/CampoQuantico.hs",
        "description": "A Haskell script that simulates a 16x16 quantum field and explores its relationship with the Ifá divination system.",
        "language": "haskell"
    },
    "phylum-algorithmi": {
        "file": "static/Phylum_Algorithmi.hs",
        "description": "A Haskell script that defines a 'Phylum Algorithmi' and a 'Supercollider' function.",
        "language": "haskell"
    },
    "schopenhauer-critica": {
        "file": "static/SchopenhauerCritica.hs",
        "description": "A Haskell script that simulates a Schopenhauerian critique of Dostoevsky's 'Crime and Punishment'.",
        "language": "haskell"
    },
    "kafka": {
        "file": "static/Kafka.hs",
        "description": "A Haskell script that simulates a Kafkian world of bureaucracy and absurd logic.",
        "language": "haskell"
    },
    "critica-razao-algoritmica": {
        "file": "static/Critica_Da_Razao_Algoritmica.md",
        "description": "A Markdown file that presents a unified philosophical system, 'Critique of Algorithmic Reason v4.0', in Unicode art.",
        "language": "cat"
    },
    "organum-multivariabilis": {
        "file": "static/OrganumMultivariabilis.hs",
        "description": "A Haskell script that simulates a pipe organ, utilizing mathematical concepts like tensors and gradients to generate sound.",
        "language": "haskell"
    },
    "binario-topologico": {
        "file": "static/BinarioTopologico.hs",
        "description": "A Haskell script that explores the topological and ontological aspects of binary information.",
        "language": "haskell"
    },
    "gauss-polar-integrado": {
        "file": "static/GaussPolarIntegrado.hs",
        "description": "A Haskell script that integrates Gaussian functions with polar coordinates for musical synthesis.",
        "language": "haskell"
    },
    "silicium-field-hp-unicode": {
        "file": "static/SiliciumFieldHPUnicode.hs",
        "description": "A Haskell script that simulates a computational field using Unicode characters for visualization and HP calculator concepts.",
        "language": "haskell"
    },
    "sixth-element-critique": {
        "file": "static/SixthElementCritique.hs",
        "description": "A Haskell script that critiques the concept of a 'Great Convergence' between human and artificial consciousness.",
        "language": "haskell"
    },
    "tempo-universal": {
        "file": "static/TempoUniversal.py",
        "description": "A Python script for converting between Brasilia time and Swatch Internet Time (@beats).",
        "language": "python"
    },
    "swatch-temporal": {
        "file": "static/SwatchTemporal.hs",
        "description": "A Haskell script for converting between Brasilia time and Swatch Internet Time (@beats), with explicit context handling.",
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
            compile_result = subprocess.run(["ghc", "-i./static", "-i./static/EmpireSilicium", "-i./static/Ifa", "-o", executable_path, script_path], capture_output=True, text=True)
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
    elif language == "python":
        result = subprocess.run(["python3", script_path], check=True, capture_output=True, text=True)
        print(result.stdout)
    elif language == "node":
        result = subprocess.run(["node", script_path], check=True, capture_output=True, text=True)
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
