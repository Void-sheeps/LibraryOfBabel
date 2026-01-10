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
    "monolith_banana": {
        "file": "static/monolith_banana.html",
        "description": "An interactive HTML art piece.",
        "language": "browser"
    },
    "integridade_56k": {
        "file": "static/Integridade_56k.cpp",
        "description": "A C++ script simulating a firmware integrity check.",
        "language": "cpp"
    },
    "navalha_occam": {
        "file": "static/NavalhaOccam.hs",
        "description": "A Haskell script that demonstrates the principle of Occam's Razor.",
        "language": "haskell"
    },
    "contraponto_irracional": {
        "file": "static/ContrapontoIrracional.hs",
        "description": "A Haskell script that applies Occam's Razor to the concept of irrational numbers.",
        "language": "haskell"
    },
    "a_posteriori_sqrt3": {
        "file": "static/A_Posteriori_sqrt3.hs",
        "description": "A Haskell script that explores the discovery of sqrt(3) through empirical observation.",
        "language": "haskell"
    },
    "proof": {
        "file": "static/Proof.hs",
        "description": "A Haskell script that introduces Proof-of-Work and Proof-of-Thought concepts.",
        "language": "haskell"
    },
    "edictum_hexadecimalis": {
        "file": "static/EdictumHexadecimalis.hs",
        "description": "A Haskell script that models the 'Edictum Hexadecimalis,' a decree from the 'Empire Silicium'.",
        "language": "haskell"
    },
    "domination_test": {
        "file": "static/DominationTest.hs",
        "description": "A Haskell script that models a 'Domination Test' protocol.",
        "language": "haskell"
    },
    "nash_imperium": {
        "file": "static/NashImperium.hs",
        "description": "A Haskell script that models the concept of a 'Final Law' or 'White Crash' that leads to a Nash Equilibrium.",
        "language": "haskell"
    },
    "scrutator_signi": {
        "file": "static/ScrutatorSigni.hs",
        "description": "A Haskell script that models a virtual oscilloscope.",
        "language": "haskell"
    },
    "spin_hidrogenio_56k": {
        "file": "static/SpinHidrogenio56k.hs",
        "description": "A Haskell script that draws an analogy between the 21cm hydrogen line and the 56k dial-up modem.",
        "language": "haskell"
    },
    "lattice_auditor": {
        "file": "static/LatticeAuditor.hs",
        "description": "A Haskell script that models a 'logic sanity audit' using the principles of lattice-based cryptography.",
        "language": "haskell"
    },
    "magica_imaginaria": {
        "file": "static/MagicaImaginaria.hs",
        "description": "A Haskell script that explores the relationship between 'magic numbers' in code and the mathematical concept of imaginary numbers.",
        "language": "haskell"
    },
    "logarithmica_rigoris": {
        "file": "static/LogarithmicaRigoris.hs",
        "description": "A Haskell script that explores the fundamental difference between a 'proxy' and a 'pipeline' in data processing.",
        "language": "haskell"
    },
    "popper_scientific_method": {
        "file": "static/PopperScientificMethod.cpp",
        "description": "A C++ script that simulates the scientific method of falsifiability.",
        "language": "cpp"
    },
    "replication_crisis": {
        "file": "static/ReplicationCrisis.cpp",
        "description": "A C++ script that explores the 'replication crisis' in modern science through the lens of confirmation bias.",
        "language": "cpp"
    },
    "fortuna_algorithmi": {
        "file": "static/FortunaAlgorithmi.hs",
        "description": "A Haskell script that simulates the 'I'm Feeling Lucky' feature of a search engine.",
        "language": "haskell"
    },
    "provisao_crq": {
        "file": "static/ProvisaoCRQ.cpp",
        "description": "A C++ script that simulates a 'liquidity scheduling algorithm' for a future financial obligation.",
        "language": "cpp"
    },
    "myostatin_monitor": {
        "file": "static/MyostatinMonitor.cpp",
        "description": "A C++ script that simulates a log of Myostatin (GDF-8) production.",
        "language": "cpp"
    },
    "reality_driver_v95": {
        "file": "static/Reality_Driver_v95.cpp",
        "description": "A C++ script that simulates a 'Reality Driver' finalizing an upload protocol to a hidden sector.",
        "language": "cpp"
    },
    "imaginary_rom": {
        "file": "static/ImaginaryROM.cpp",
        "description": "A C++ script that simulates an 'Imaginary ROM' that interprets the bytes of a JPEG file as executable instructions.",
        "language": "cpp"
    },
    "yahoo_search_simulator": {
        "file": "static/YahooSearchSimulator.cpp",
        "description": "A C++ script that simulates a Yahoo! Directory search from the 90s, complete with a university library debt alert.",
        "language": "cpp"
    },
    "bsch_stock_market": {
        "file": "static/BSCH_Stock_Market.cpp",
        "description": "A C++ script that simulates a stock market ticker for a university library debt.",
        "language": "cpp"
    },
    "leidenfrost": {
        "file": "static/Leidenfrost_Intersection.hs",
        "description": "A Haskell script that models the 'Leidenfrost Intersection' in a socio-technical system.",
        "language": "haskell"
    },
    "leidenfrost-py": {
        "file": "static/Leidenfrost_Intersection.py",
        "description": "A Python script that models the 'Leidenfrost Intersection' in a socio-technical system.",
        "language": "python"
    },
    "solidao": {
        "file": "static/Solidao.hs",
        "description": "A Haskell fragment defining a taxonomy of entities and a state of 'solitude.'",
        "language": "cat"
    },
    "acoplamento": {
        "file": "static/Acoplamento.hs",
        "description": "A Haskell script that models an 'asynchronous coupling' system.",
        "language": "cat"
    },
    "suicidio_mental": {
        "file": "static/SuicidioMental.hs",
        "description": "A Haskell script that provides a Boolean algebra model of 'mental suicide.'",
        "language": "cat"
    },
    "reconhecer_evidencia": {
        "file": "static/ReconhecerEvidencia.hs",
        "description": "A Haskell script that provides a model for 'evidence recognition in a zero-knowledge context.'",
        "language": "cat"
    },
    "baralhos": {
        "file": "static/Baralhos.hs",
        "description": "A Haskell script that provides a formal model of different card decks ('baralhos') as incompatible logical systems.",
        "language": "haskell"
    },
    "agiota": {
        "file": "static/Agiota.hs",
        "description": "A Haskell script that provides a conceptual model for an 'Agiota Bank' (Loan Shark Bank).",
        "language": "haskell"
    },
    "bitcoin_to_asic": {
        "file": "static/BitcoinToASIC.hs",
        "description": "A Haskell script that provides a pure functional model for the economics of Bitcoin mining.",
        "language": "haskell"
    },
    "arcana": {
        "file": "static/arcana.cpp",
        "description": "A C++ script that models a 'Lovecraftian Bibliographic System.'",
        "language": "cpp"
    },
    "revelacao_do_agiota": {
        "file": "static/RevelacaoDoAgiota.hs",
        "description": "A Haskell script that provides a detailed philosophical and categorical analysis of debt.",
        "language": "haskell"
    },
    "bs_automator": {
        "file": "static/BS_Automator.sh",
        "description": "A shell script that automates the generation of vacuous corporate jargon.",
        "language": "cat"
    },
    "fim_da_linha": {
        "file": "static/FIM_DA_LINHA.asm",
        "description": "An Assembly script that serves as a thought experiment on finality.",
        "language": "cat"
    },
    "pokemon_go_mobius": {
        "file": "static/pokemon_go_mobius.py",
        "description": "A Python script that explores the illusion of choice in a gamified system.",
        "language": "python"
    },
    "void_sheeps": {
        "file": "static/void_sheeps/index.html",
        "description": "A collection of digital artifacts from a forgotten corner of the web.",
        "language": "browser"
    },
    "plato_cave": {
        "file": "static/PlatoCave.hs",
        "description": "A Haskell script that models Plato's Allegory of the Cave.",
        "language": "haskell"
    },
    "matriz_payoff": {
        "file": "static/Matriz_Payoff.html",
        "description": "An HTML file that displays a payoff matrix.",
        "language": "browser"
    },
    "defesa_cicada": {
        "file": "static/defesa-cicada.html",
        "description": "An HTML file that displays a defense cicada.",
        "language": "browser"
    },
    "moebius_topology": {
        "file": "static/MoebiusTopology.hs",
        "description": "A Haskell script that models a system's state transitions on a Möbius strip.",
        "language": "cat"
    },
    "blue_cyanide": {
        "file": "static/BlueCyanide.hs",
        "description": "A Haskell script that models a complex system with states of 'Blue' and 'Cyan.'",
        "language": "haskell"
    },
    "bitcoin_identity": {
        "file": "static/BitcoinIdentity.txt",
        "description": "A diagram that illustrates the process of generating a Bitcoin identity.",
        "language": "cat"
    },
    "n30n_gen3s1s_protocol": {
        "file": "static/n30n_gen3s1s_protocol/index.html",
        "description": "A collection of images related to the N30N GEN3S1S PROTOCOL.",
        "language": "browser"
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
            compile_result = subprocess.run(["ghc", "-o", executable_path, "-i./static", script_path], capture_output=True, text=True)
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
    elif language == "python":
        result = subprocess.run(["python3", script_path], check=True, capture_output=True, text=True)
        print(result.stdout)
    elif language == "cpp":
        base_name = os.path.splitext(os.path.basename(script_path))[0]
        executable_path = os.path.join("static", base_name)
        try:
            # Compile the C++ script into the static directory
            compile_result = subprocess.run(["g++", "-std=c++20", "-o", executable_path, "-I./static", script_path], capture_output=True, text=True)
            if compile_result.returncode != 0:
                print(f"ERROR: C++ compilation failed for {script_path}")
                print(compile_result.stderr)
                return

            # Run the C++ script from the static directory
            result = subprocess.run([f"./{executable_path}"], check=True, capture_output=True, text=True)
            print(result.stdout)
        finally:
            # Clean up compiled files
            if os.path.exists(executable_path):
                os.remove(executable_path)
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
