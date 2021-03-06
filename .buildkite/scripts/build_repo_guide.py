#!/usr/bin/env python

import os

from sbt_dependency_tree import Repository


def build_repo_guide(libraries, services):
    lines = [
        "# Project guide",
        "",
        "This is an autogenerated list of all the projects in this repo.",
        "",
        "Here's what's in each project directory:",
        "- `src/main` = source code for this project",
        "- `src/test` = the tests for this project",
        "- `docker-compose.yml` (optional) = the Docker containers we run while testing this project, if any",
        "- `Dockerfile` = the Dockerfile used to build this project, if it's a service we run in ECS",
    ]

    if libraries:
        lines.extend(["", "## Libraries", ""])

        for name, project in sorted(libraries.items()):
            lines.append(
                f"* [**{name}**](../../{project.folder}) – {project.description}"
            )

    if services:
        lines.extend(["", "## Services", ""])

        for name, project in sorted(services.items()):
            lines.append(
                f"* [**{name}**](../../{project.folder}) – {project.description}"
            )

    return "\n".join(lines)


if __name__ == "__main__":
    repo = Repository(".sbt_metadata")

    services = {
        name: project
        for name, project in repo.projects.items()
        if os.path.exists(os.path.join(project.folder, "Dockerfile"))
    }

    libraries = {
        name: project for name, project in repo.projects.items() if name not in services
    }

    with open("docs/developers/project-guide.md", "w") as outfile:
        outfile.write(build_repo_guide(libraries=libraries, services=services))
