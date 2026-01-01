
import asyncio
import aiohttp
import time
from typing import List, Dict, Any, Tuple

# --- Configuration ---
# A list of services to be audited. Each service is a dictionary.
SERVICES: List[Dict[str, str]] = [
    {'name': 'Auth Service', 'url': 'https://api.github.com/events'},  # Using a reliable public API for demo
    {'name': 'Payment Gateway', 'url': 'https://api.stripe.com/v1/charges'}, # Another reliable public API
    {'name': 'Legacy Core', 'url': 'http://localhost:8080/ping'}  # This is expected to fail
]

# --- High-Order Function: The Technical Command ---
async def check_service(session: aiohttp.ClientSession, service: Dict[str, str]) -> Dict[str, Any]:
    """
    Checks the status of a single service.

    Args:
        session: The aiohttp client session.
        service: A dictionary containing the service's name and URL.

    Returns:
        A dictionary with the service's name and status.

    Raises:
        Exception: If the service is unreachable or returns an error.
    """
    name, url = service['name'], service['url']
    try:
        # HEAD request with a 2-second timeout
        async with session.head(url, timeout=2) as response:
            # aiohttp raises an exception for non-2xx statuses if raise_for_status() is called
            response.raise_for_status()
            return {'name': name, 'status': 'ONLINE'}
    except Exception as err:
        # Re-raise with a more descriptive message to be captured by asyncio.gather
        raise Exception(f"{name} unreachable: {err}") from err

async def run_audit(targets: List[Dict[str, str]]) -> Dict[str, List[Any]]:
    """
    Executes a health audit on multiple endpoints simultaneously.

    Args:
        targets: A list of service dictionaries to audit.

    Returns:
        A dictionary reporting the active services and failures.
    """
    print("Running health audit...")
    start_time = time.time()

    async with aiohttp.ClientSession() as session:
        # Map the list of services to a list of tasks (coroutines)
        tasks = [check_service(session, service) for service in targets]

        # Execute all tasks concurrently and wait for them to complete
        # return_exceptions=True is the equivalent of JavaScript's Promise.allSettled
        results = await asyncio.gather(*tasks, return_exceptions=True)

    # Process results, partitioning them into successes and failures
    report: Dict[str, List[Any]] = {'active': [], 'failures': []}
    for result in results:
        if isinstance(result, Exception):
            report['failures'].append(str(result))
        else:
            report['active'].append(result)

    duration = time.time() - start_time
    print(f"Audit Duration: {duration:.2f}s")
    return report

# --- Execution ---
if __name__ == "__main__":
    # In Python, the top-level await is not standard, so we run the async function
    # using asyncio.run()
    audit_report = asyncio.run(run_audit(SERVICES))

    # Pretty print the report
    print("\n--- Health Audit Report ---")
    print("\n[ Active Services ]")
    if audit_report['active']:
        for service in audit_report['active']:
            print(f"- {service['name']}: {service['status']}")
    else:
        print("No services are active.")

    print("\n[ Failures ]")
    if audit_report['failures']:
        for failure in audit_report['failures']:
            print(f"- {failure}")
    else:
        print("No failures recorded.")
    print("\n---------------------------\n")
