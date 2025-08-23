# 🎤 Transparent Talent Contest

A decentralized talent contest platform built on Stacks blockchain, ensuring fair voting and automatic prize distribution for singers, comedians, and performers! 🌟

## ✨ Features

- 🎵 **Talent Showcase**: Singers and comedians can submit their performances
- 🗳️ **Transparent Voting**: On-chain voting prevents rigging and manipulation
- 💰 **Automatic Prizes**: Smart contract automatically distributes rewards to winners
- 🏆 **Leaderboards**: Real-time contest rankings and statistics
- 🔒 **Tamper-Proof**: All votes and results stored immutably on blockchain
- 💎 **Entry Fees**: Optional entry fees to build prize pools

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet (Hiro Wallet recommended)

### Quick Start

1. **Create a Contest** 🎯
```clarity
(contract-call? .Transparent-Talent-Contest create-contest 
  "Summer Singing Competition" 
  "singing" 
  u1000  ;; duration in blocks (~1 week)
  u1000000  ;; entry fee in microSTX (1 STX)
  u70  ;; winner gets 70%
  u20) ;; runner-up gets 20%
```

2. **Submit Your Entry** 🎭
```clarity
(contract-call? .Transparent-Talent-Contest submit-entry 
  u1  ;; contest-id
  "My Amazing Song" 
  "A heartfelt ballad about blockchain dreams"
  "https://ipfs.io/ipfs/QmYourAudioFile")
```

3. **Vote for Talent** 🗳️
```clarity
(contract-call? .Transparent-Talent-Contest vote-for-entry 
  u1  ;; contest-id
  u1) ;; entry-id
```

4. **Finalize & Claim Prizes** 🏆
```clarity
;; Contest creator determines winners after voting ends
(contract-call? .Transparent-Talent-Contest determine-winners u1 u5 (some u3))

;; Winner claims prize
(contract-call? .Transparent-Talent-Contest claim-winner-prize u1)

;; Runner-up claims prize  
(contract-call? .Transparent-Talent-Contest claim-runner-up-prize u1)
```

## 📊 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-contest` | Start a new talent contest | title, category, duration, entry-fee, winner%, runner-up% |
| `submit-entry` | Submit your performance | contest-id, title, description, media-url |
| `vote-for-entry` | Vote for your favorite entry | contest-id, entry-id |
| `determine-winners` | Manually set contest winners | contest-id, winner-entry-id, runner-up-entry-id |
| `claim-winner-prize` | Claim winner prize | contest-id |
| `claim-runner-up-prize` | Claim runner-up prize | contest-id |
| `claim-creator-fee` | Claim organizer fee | contest-id |
| `emergency-withdraw` | Creator emergency fund recovery | contest-id |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-contest` | Get contest details |
| `get-entry` | Get entry information |
| `get-contest-stats` | View contest statistics |
| `get-contest-winner` | Check contest winner |
| `get-contest-runner-up` | Check contest runner-up |
| `get-prize-info` | View prize distribution |
| `has-voted` | Check if user voted |
| `is-contest-active` | Check if contest is ongoing |
| `is-winner` | Check if user is winner |
| `is-runner-up` | Check if user is runner-up |

## 🎯 Usage Examples

### 🎤 For Performers
1. Find an active contest using `get-current-contest-id`
2. Submit your entry with performance details
3. Share your entry-id with fans for votes
4. Claim your prize if you win! 

### 🗳️ For Voters  
1. Browse active contests and entries
2. Vote for your favorite performance
3. Track results on the leaderboard

### 🎪 For Contest Organizers
1. Create contests with custom prize structures
2. Set entry fees to build prize pools  
3. Finalize contests when voting ends
4. Collect creator fees from remaining funds

## 💡 Prize Distribution

The smart contract automatically calculates prize distribution:
- **Winner**: Gets the percentage set by contest creator
- **Runner-up**: Gets the runner-up percentage  
- **Creator**: Receives remaining funds as organizer fee
- **Transparent**: All calculations done on-chain

## 🔒 Security Features

- ✅ One vote per user per contest
- ✅ Immutable vote records
- ✅ Automatic prize distribution
- ✅ Contest creator controls
- ✅ Emergency withdrawal safeguards
- ✅ Input validation and error handling

## 🛠️ Development

### Testing
```bash
clarinet test
```

### Type Checking  
```bash
clarinet check
```

### Console Testing
```bash
clarinet console
```

## 📝 Contract Address

Deploy to testnet/mainnet and update this section with your contract address.

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Test your changes with `clarinet check`
4. Submit a pull request

## 📜 License

MIT License - Build amazing talent contests! 🚀

---

**Made with ❤️ for the creator economy on Stacks blockchain**
