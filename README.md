# 🚜 Decentralized Equipment Rental Platform

A blockchain-based solution for peer-to-peer agricultural equipment rental using Stacks blockchain.

## 🎯 Features

- List farming equipment for rent
- Rent equipment using STX tokens
- Track rental periods and availability
- Manage equipment returns

## 💡 How It Works

### For Equipment Owners
1. List your equipment with a daily rate
2. Receive STX payments automatically
3. Track who's using your equipment

### For Renters
1. Browse available equipment
2. Rent equipment by paying in STX
3. Return equipment when done

## 🛠 Contract Functions

### list-equipment
```clarity
(list-equipment equipment-id name daily-rate)
```

### rent-equipment
```clarity
(rent-equipment equipment-id days)
```

### return-equipment
```clarity
(return-equipment equipment-id)
```

## 📊 Read-Only Functions

- `get-equipment`: View equipment details
- `get-user-rentals`: Check user's active rentals

## 🔒 Security

- Ownership verification
- Automatic payment processing
- Rental period enforcement

## 🚀 Getting Started

1. Deploy the contract using Clarinet
2. Use contract functions through the Stacks wallet
3. Monitor transactions on the Stacks explorer

## 💪 Contributing

Feel free to submit issues and pull requests!
```

