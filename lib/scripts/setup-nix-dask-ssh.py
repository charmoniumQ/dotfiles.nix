import subprocess
import shlex
import typer
import pathlib
import itertools
import asyncio
import rich.console
import rich.progress
import rich.padding
import dataclasses
import charmonium.async_subprocess


console = rich.console.Console()
progress = rich.progress.Progress(
    *rich.progress.Progress.get_default_columns(),
    rich.progress.MofNCompleteColumn(),
    console=console,
)


flatten1 = lambda list_of_lists: list(itertools.chain(*list_of_lists))


def main(
        cmd: list[str],
        flake_uri: str,
        hosts_file: pathlib.Path,
        hosts: list[str] = [],
) -> None:
    with progress:
        asyncio.run(amain(cmd, flake_uri, hosts_file, hosts))


async def amain(
        cmd: list[str],
        flake_uri: str,
        hosts_file: pathlib.Path,
        hosts: list[str] = [],
) -> None:
    hosts = hosts_file.read_text().splitlines() + flatten1([host.split(",") for host in hosts])
    awaitables = [
        configure_host(cmd, flake_uri, host)
        for host in hosts
    ]
    for coro in progress.track(asyncio.as_completed(awaitables), total=len(hosts)):
        host, exc = await coro
        if exc is not None:
            console.print(
                "[red]"
                f"{host} failed (rc={exc.returncode}): "
                f"{rich.markup.escape(shlex.join(exc.args2))}"
                "[/red]"
            )
            console.print(rich.padding.Padding(rich.markup.escape(
                exc.stdout.decode("replace") + exc.stderr.decode("replace")
            ), (0, 2)))
        else:
            console.print(f"[green]{host} deployed[/green]")


async def configure_host(
        cmd: list[str],
        flake_uri: str,
        host: str,
) -> tuple[str, charmonium.async_subprocess.CalledProcessError | None]:
    if flake_uri:
        try:
            await charmonium.async_subprocess.run(
                [
                    "ssh",
                    host,
                    "which nix || (curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install)",
                ],
                capture_output=True,
                check=True,
            )
        except charmonium.async_subprocess.CalledProcessError as exc:
            return host, exc
        try:
            await charmonium.async_subprocess.run(
                ["nix", "copy", "--to", f"ssh-ng://{host}", flake_uri],
                capture_output=True,
                check=True,
            )
        except charmonium.async_subprocess.CalledProcessError as exc:
            return host, exc

    if cmd:
        try:
            await charmonium.async_subprocess.run(
                ["ssh", host, cmd],
                capture_output=True,
                check=True,
            )
        except charmonium.async_subprocess.CalledProcessError as exc:
            return host, exc

    return host, None


if __name__ == "__main__":
    typer.run(main)
