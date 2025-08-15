# EsusuVault.clar

## Cooperative Savings (Esusu / Ajo) Smart Contract for Stacks

A Clarity smart contract for managing a rotating savings group (Esusu/Ajo) on the Stacks blockchain. This contract allows a group of members to contribute funds in cycles, with each member taking turns to claim the pooled savings.

---

## Features

- **Admin Controls:** Only the contract deployer (admin) can add or remove members, set contribution amounts, and start the group.
- **Member Actions:** Members can contribute funds and claim the pot when it’s their turn.
- **Penalty System:** Admin can assign penalties for late contributions.
- **Cycle Management:** Tracks cycles, member turns, and contributions.
- **Safety Guards:** Maximum group size and strict error handling to prevent misuse.

---

## Usage

### Deployment

Deploy the contract using [Clarinet](https://docs.stacks.co/clarity/clarinet) or your preferred Stacks tool. The deployer becomes the admin.

### Admin Functions

- `add-member(who)` – Add a new member (principal).
- `remove-member(who)` – Remove a member by principal.
- `set-contribution-amount(amount)` – Set the contribution amount per cycle.
- `set-cycle-length(blocks)` – Set the cycle length in blocks.
- `start-group()` – Start the savings group.

### Member Functions

- `contribute()` – Contribute the required amount for the current cycle.
- `claim-pot()` – Claim the pooled savings when it’s your turn.

### Penalty Functions

- `assign-penalty(who, amount)` – Assign a penalty to a member.

---

## Development & Testing

- Written in Clarity for Stacks.
- Designed for use with [Clarinet](https://docs.stacks.co/clarity/clarinet) for local testing.
- All logic is bounded for safety and auditability.

---

## File Structure

- `contracts/Esusu-vault.clar` – Main smart contract.
- `.gitignore` – Standard ignores for logs, node modules, and build artifacts.

ce implementation. Please audit and test thoroughly before deploying on Mainnet.
- For questions or contributions, open an issue or pull request on GitHub.
